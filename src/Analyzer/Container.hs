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
                           let cpu = value node
                           --cpu'' <- if ((ppoint . sink) rel, (ppoint . source) rel ) == (14,13)
                           --             then setCompl (cpu {active = (0,1) })
                           --             else return cpu

                           let cpu' = unsafePerformIO $ do
                                          if (ipoint label) == 13
                                             then do
                                                  putStrLn $ "FOUND AT= " ++ show (ipoint label)
                                                  --putStrLn $ "STABLE AT= " ++ show (stableLoop node)
                                                  putStrLn $ "CERT AT= " ++ show (cert)
                                                  return cpu
                                             else return cpu

                           return cpu { context = contexts node, stable = stableLoop node }
              Nothing  -> do
                         --let label' = reset label

                         let rel' = unsafePerformIO $ do
                                        putStrLn $ "RE-LABEL AT = " ++ show (label) -- , label')
                                        return rel

                         {-if (ipoint label) == (11,16,4)
                             then do
                                  (cpu, cert') <- read cert (adjustIn rel' label')
                                  cert'' <- store cert' (adjustOut rel label) cpu
                                  --error $ show (keys cert'')
                                  return (cpu, cert)

                             else read cert (adjustIn rel' label')-}


                         read cert (adjustIn rel label)


    read_ cert label
      = case  lookup (ipoint label) cert of
              Just node ->  do
                           let cpu = value node

                           let cpu' = unsafePerformIO $ do
                                         putStrLn $ "GET_ AT= " ++ show label
                                         return cpu
                           return cpu { context = contexts node }
              Nothing  -> do
                         --let bottom' = unsafePerformIO $ do
                         --                putStrLn $ "NOT FOUND AT= " ++ show label
                         --                return bottom

                         return bottom


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
                               {-let var_ = unsafePerformIO $ do
                                            if ppoint source == 14
                                               then
                                                    do
                                                    putStrLn $ "STATE " ++ show (ctxs'')
                                                    return var'
                                               else return var'-}
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

         let cert_ = unsafePerformIO $ do
                       --putStrLn $ "STORE AT= " ++ show (ipoint sink) ++ " ;CONTEXTS= " ++ show st
                       --putStrLn $ "STORE AT= " ++ show (ipoint sink) ++ " ;INFEASIBLE= " ++ show infeasibleArch
                       --putStrLn $ "STORE AT= " ++ show (ipoint sink) ++ " ;ACTIVE= " ++ show active
                       if ppoint source == 14 || ppoint source == 13
                          then do
                               putStrLn $ "STORE AT= " ++ show (ipoint sink) ++ " ;STABLE= " ++ (show (active cpu'))
                               putStrLn $ "STORE AT= " ++ show (ipoint sink) ++ " ;STABLE= " ++ (show cert''')
                          else return ()
                       return cert'''

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
            --(prev, True ) -> return $ (succ (ppoint source)) : contexts
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
    --putStrLn $ "ifDeleteCContext: contexts=" ++ show contexts ++ " ,caller=" ++ show caller
    case (caller, exit sink) of
         (prev, False) -> return contexts
         (prev, True ) -> case contexts of
                              --c:cs -> return cs
                              c:cs -> if null $ takeWhile (== c) cs
                                        then return cs
                                        else return cs -- $ dropWhile (==c) cs
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
                   True -> do
                          --putStrLn $ "RETURN CALLING CONTEXT= " ++ show context
                          --putStrLn $ "EXIT CONTEXT= " ++ show (ipoint sink)
                          case context of
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
         ExitL l ->  do
                    --putStrLn $ "UPDATE CALLING CONTEXT= " ++ show (sink rel, context)
                    case context of
                         [] -> return cpu
                         c:[] -> return cpu { context = c:[] }
                         c:cs -> if null $ takeWhile (== c) cs
                                   then  do
                                         --putStrLn "NO RECURSIVE CALLS"
                                         --return cpu { context = cs }
                                         return cpu { context = c:cs }
                                   else return cpu
         _ -> return cpu

