-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Container
-- Copyright   :  None
-- License     :  BSD3
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analyzer.Container ( read, read_, store, updateCallingContext, returnCallingContext,
                            ifNewCContext, ifDeleteCContext

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Prelude hiding (lookup, read)
import Data.Map hiding (null)
import Data.Maybe
import qualified Data.List as List
import System.IO.Unsafe
import Control.DeepSeq
import System.IO.Unsafe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Pipeline hiding (reset, infeasible)
import Arm.Instruction
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.Certificate
import Analyzer.Semantics hiding (infeasible)


-- | The Container type class defines the access to the invariants map. The read_ function is
--   the non-blocking version of read. The store function also updates the calling contexts.
class (Lattice b) => Container a b where
  read_ :: (Show l) => (Labeled l) => a -> l -> IO b
  read :: (Transition r) => a -> r -> IO b
  store :: (Transition r) => a -> r -> b -> IO a


instance (Lattice (CPU a), Show a, Eq a, Cost a) =>
         Container (Invs (CPU a)) (CPU a) where
    read cert rel
      = do
        let label = source rel
        case  lookup (ipoint label) cert of
              Just node ->  do
                            --let cpu = value node
                            return (value node) { context = contexts node, stable = stableLoop node }
              Nothing  -> read cert (adjustIn rel label)


    read_ cert label
      = case  lookup (ipoint label) cert of
              Just node -> do
                           let cpu = value node
                           return cpu { context = contexts node }
              Nothing  ->  return bottom


    store cert rel cpu@CPU {context = ctxs, active = act, stable, multi}
      =  do
         let core = multi ! (snd act)
             infeasibleArch = infeasiblePath (registers core)
         cpu' <- clearCompl cpu
         let (sink, source) = ppoints rel
         st <- case  lookup (ipoint sink) cert of
                  Just node ->  do

                               let caller = contexts node
                               let ctxs'  = unsafePerformIO $ ifNewCContext caller rel ctxs
                                   ctxs'' = unsafePerformIO $ ifDeleteCContext caller rel ctxs'
                               return ctxs''
                  Nothing -> do
                            let ctxs'  = unsafePerformIO $ ifNewCContext [] rel ctxs
                                ctxs'' = unsafePerformIO $ ifDeleteCContext [] rel ctxs'
                            return ctxs''


         let node = Node {  value = cpu', stableFixpoint = False,
                            stableValue = [], insideLoop = No,
                            contexts = st, redirect = Nothing, stableLoop = False,
                            infeasibleNode = False }

         let cert'  = if member (ipoint sink) cert && not infeasibleArch
                         then adjust (\n -> updateValue n cpu' st) (ipoint sink) cert
                         else insert (ipoint sink) node cert
             cert'' = if exit sink && not infeasibleArch
                         then adjust (\n -> updateRedirects n ctxs) (ipoint source) cert'
                         else cert'
             cert''' = if infeasibleArch
                         then adjust (\n -> n {infeasibleNode = True} ) (ipoint sink) cert''
                         else adjust (\n -> n {infeasibleNode = False} ) (ipoint sink) cert''
         return cert'''


updateValue
  :: Node (CPU a)
  ->  (CPU a)
  ->  [Context]
  ->  Node (CPU a)

updateValue node cpu st
  = node { value = cpu, contexts = st, stableLoop = stable cpu }


-- |
updateRedirects
  :: Node a
  ->  [Context]
  ->  Node a

updateRedirects node@Node { redirect } ctxs
  = case redirect of
         Nothing -> node { redirect = Just ctxs }
         Just r  -> node { redirect = Just (List.nub (r ++ ctxs)) }


-- | Add a calling context after a procedure call.
ifNewCContext
  :: Transition r => [Context]
  ->  r
  ->  [Context]
  ->  IO [Context]

ifNewCContext caller rel contexts
  = let (sink, source) = ppoints rel
    in case (caller, bl sink) of
            (prev, False) -> return $ contexts
            (prev, True ) -> return $ (succ (source)) : contexts


-- | Drop the top element of a calling context after a procedure exit.
ifDeleteCContext
  :: Transition r => [Context]
  ->  r
  ->  [Context]
  ->  IO [Context]

ifDeleteCContext caller rel contexts
  = do
    let (sink, source) = ppoints rel
    case (caller, exit sink) of
         (prev, False) -> return contexts
         (prev, True ) -> case contexts of
                              c:cs -> if null $ takeWhile (== c) cs
                                        then return cs
                                        else return cs
                              [] -> return prev

-- | Update the sink label of a transition with the context of a CPU
returnCallingContext
  :: (Transition r) => r
  ->  CPU a
  ->  IO r

returnCallingContext rel cpu@CPU { context }
  =  do
     let (sink, source) = ppoints rel
     sink' <- case exit sink of
                   False -> return $ sink
                   True  -> case context of
                                  (c:cs) -> return $ setId sink (ppoint c)
                                  [] -> return $ setId sink (-1)
     return $ adjustOut rel sink'



-- | Update the calling context of a CPU. Drop the top element of the calling context
--   if not inside a recursive procedure.
updateCallingContext
  :: (Transition r) =>  r
    -> CPU a
    -> IO (CPU a)

updateCallingContext rel cpu@CPU { context }
  = case sink rel of
         ExitL l -> case context of
                         [] -> return cpu
                         c:[] -> return cpu { context = c:[] }
                         c:cs -> if null $ takeWhile (== c) cs
                                   then return cpu { context = c:cs }
                                   else return cpu
         _ -> return cpu

