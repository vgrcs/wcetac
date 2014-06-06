-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ProgramFlow
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
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP #-}
module Analyzer.ProgramFlow ( Iterable (..),   Forkable (..), instrument

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Prelude hiding (lookup)
import System.IO.Unsafe
import qualified Data.List as List
import Data.Map -- hiding (adjust)
import Data.Maybe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.Certificate hiding (iterations)
import Analyzer.Container
import Analyzer.Lattice
import Analyzer.ValueAbstraction
import Analyzer.Label hiding (labelId)
import Analyzer.ARM5StagePipeline
import Arm.CPU
import Arm.Register
import Arm.RegisterName
import Arm.Pipeline


instrument
  :: (Show a, Eq a, Transition r) =>
  St (CPU a)
  ->  (r, Invs (CPU a))
  ->  IO (St (CPU a))


instrument  s@St{ edges }  (r', cert')
  =  do
     let (cert'', le') = unsafePerformIO $ iterations r' cert' edges
     return s { labelSt = sink r', invs = cert'', edges = le'}


iterations
  :: (Transition r, Show a) => r
  -> (Invs (CPU a))
  -> EdgeCount
  -> IO (Invs (CPU a), EdgeCount)


iterations r cert le
  = do
    let lookId = (ipoint . source) r
        storeId = (ipoint . sink) r
        read = cert ! lookId
        store = cert ! storeId

    let cert' = case insideLoop read of
                     Yes a -> let node' = inLoop store (a ++ (sink r):[])
                              in insert storeId node' cert
                     No -> cert
        fixed = elem DataStable (stableValue store)
        infeasible =  infeasibleNode store
        update n = if fixed || infeasible then n else succ n
        le' = if member (lookId, storeId) le
                 then adjust update (lookId, storeId) le
                 else if  infeasible
                          then insert (lookId, storeId) (TopL 0) le
                          else insert (lookId, storeId) (TopL 1) le

    return (cert, le')


inLoop
  :: Node a
  -> [Label]
  -> Node a

inLoop n@Node { insideLoop = No } headAndJump
  = n { insideLoop =  Yes ( List.nub headAndJump ) }
inLoop n@Node { insideLoop = Yes a } headAndJump
  = n { insideLoop = Yes ( List.nub (a ++ headAndJump)) }



class (Show a) => Iterable a where
   fixpoint  :: a -> IO Bool
   emptyStack  :: a -> IO Bool
   loop   :: (Stateable b) => b -> a -> IO a

class (Show a) => Forkable a where
   branch :: a ->  IO Bool
   compl :: a -> IO a

instance (Show a,  Eq a, Cost a, Ord a) => Forkable (St (CPU a))  where

    branch s
      = do
        let (after, cert) = (getLabel s, invs s)
            cpu = value $ cert ! (ipoint after)
            c = (snd . active) cpu
            core = (multi cpu) ! c
            CtrVal status = getReg (registers core) CPSR
            b = getControlB (control status)
        case b of
             1 -> return True
             0 -> return False

    compl s
       = do let (at,cert) = (labelSt s, invs s)
                node = cert ! (ipoint at)
            cpu' <- setCompl (value node)
            let node'  = node { value = cpu', insideLoop = No, stableLoop = False,
                                stableFixpoint = False, stableValue = [] }
                cert' = insert (ipoint at) node' cert
            return s { invs = cert' }


instance (Show a,  Eq a, Cost a, Ord a, Show (Node (CPU a) )) =>
          Iterable (St (CPU a))  where
    fixpoint s'
       = do
         b <- branch s'
         let (at,cert) = (getLabel s', invs s')
             Just (n@Node { stableFixpoint = fix, stableValue = value, stableLoop = loop } )
                 = lookup (ipoint at) cert

             (fix',b') = (loop || fix, b)

         return $ not fix' && b'

    emptyStack s
       = do
         let (at,cert) = (getLabel s, invs s)
             c:cs = contexts $ cert ! (ipoint at)
             recursive = (procName . parent) c == (procName . parent) at
             cond = recursive

         return $ not cond

    loop entry out
       = do  let (after, cert) = (getLabel out, invs out)
                 at = getLabel entry
                 head = ppoint at
                 jump = ppoint after
                 start' = setId at head

             let update k l = if k == head  ||  k == jump
                                 then inLoop l [start' , after]
                                 else l

                 cert' = mapWithKey update cert
                 out' = out  {invs = cert' }
             becomeFeasible out'





