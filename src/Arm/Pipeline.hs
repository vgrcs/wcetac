-----------------------------------------------------------------------------
--
-- Module      :  Arm.Pipeline
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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Arm.Pipeline  where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.List
import Data.Word
import Data.Map
import qualified Data.Vec as Vec
import Data.Number.PartialOrd
import System.IO.Unsafe
import Data.Binary

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Memory
import Arm.Register
import Arm.Instruction
import Analyzer.Lattice
import Analyzer.Certificate

-- | The Pipeline abstract domain is a set of "concrete" pipeline states where
--   the resources registers and memory are abstract domains adjoined with a timing property.
type Pipeline a = [PState a]

-- | Concatenation of sets of states
instance (Eq a, Ord (PState a), Show (PState a)) => Lattice (Pipeline a) where
  bottom = []
  join  new existing
       = (sort) (existing ++ new)

instance (Eq a, Ord a, Cost a) => PartialOrd (Pipeline a) where
  cmp a b
    = if isInfixOf a b || isSuffixOf a b  || isPrefixOf a b
         then Just LT
         else if a == b then Just EQ
              else Just GT


-- | The "concrete" pipeline state has the next program counter to fetch, the internal state of
--   resources (cpsr), a copy of the Registers and the Memory, plus a coordinates vector with
--   pipeline tasks.
data PState a = PState { simtime :: Int,
                         nextpc :: Word32,
                         cpsr :: Word32,
                         regfile :: Registers,
                         mem :: Memory,
                         shared :: SharedMemory,
                         coords :: Coord a ,
                         targets :: Targets,
                         final :: Bool,
                         stableP :: Bool,
                         parallelP :: Bool,
                         busytime :: Double,
                         busyPeriods :: (Int, Int)  }

type Targets = [(Instruction, Word32)]

-- |
instance (Eq a,  Cost a, Ord a) =>  Eq (PState a) where
  a == b = coords a == coords b




-- | Abstract task states are "concrete" states adjoined with a cost type variable a.
data AbsTaskState a = AbsTaskState { property :: a,
                                     stage :: Stage,
                                     task :: TaskState } -- deriving (Eq)



instance (Eq a, Cost a) => Eq (AbsTaskState a) where
  a == b = let  p1 = (relative . property) a == (relative . property)  b
                p2 = (busdelay . property) a == (busdelay . property)  b
                s = stage a == stage b
                t = task a == task b
          in s && t && p1


-- | For a pipeline with 3 lines, there is coordinates vector with 3 task abstract states.
data Coord a = Coord (Vec.Vec3 (AbsTaskState a))


-- | The coordinates may be the same even if they are in different lines inside the pipeline.
instance (Eq a, Ord a, Cost a) => Eq (Coord a) where
  Coord a == Coord b
    =  -- length ((Vec.toList a) `intersect` (Vec.toList b)) == Vec.length a
       (maxcycles (Coord a)) == (maxcycles (Coord b))


instance (Ord a, Eq a, Show (AbsTaskState a), Cost a) => Ord (Coord a) where
  compare a b = compare (maxcycles a) (maxcycles b)

-- | Define a Cost variable
class Cost a where
  emptyCost :: a
  start :: Int -> Int -> a
  reset :: Int -> Double -> a -> a
  flushed :: Int -> a -> a
  update :: Int -> Double -> a -> a
  infeasible :: a -> a
  fetchFailed :: a -> a
  sharedAccess :: a -> IO a
  fetchedInstr :: a -> a
  structHazard :: a -> a
  decodedOps  :: a -> a
  dataHazard :: a -> a
  executedALU  :: a -> a
  memoryExchange  :: a -> a
  writeBack :: a -> a
  constantBound :: Int -> a -> a
  relative :: a -> Int
  prevSharedAccess :: a -> Maybe Double
  busdelay :: a -> Int
  isBusy :: a -> Bool



-- | Extracts from the pipeline states in the WriteBack (WB) phase the maximum of elapsed cycles
maxcycles
  :: (Cost a, Ord a)
  =>  (Coord a)
  ->  a

maxcycles (Coord state)
  = let count = (\ node -> case node of { task@AbsTaskState { property = c, stage = WB } -> c; _ -> emptyCost})
        cycles = Vec.map count state
    in Vec.maximum cycles


-- | This function takes a cost property and an actual task state and produces a
--   new abstract task state.
type AbsTask a = a -> (TaskState) -> IO (AbsTaskState a)

-- | The vector of transfer functions
type FunArray a =  Vec.Vec3 (AbsTask a)

-- | The five stages of a pipeline
data Stage = FI | DI | EX | MEM | WB  deriving (Eq,Show, Ord)

-- | A task for a given Instruction carries the updated next PC, the actual state of resources,
--   and the main resources from which it reads and writes.
data Task = Task { taskInstr :: Instruction,
                   taskNextPc :: Word32,
                   taskCpsr :: Word32,
                   taskRegisters :: Registers,
                   taskMemory :: Memory,
                   taskShared :: SharedMemory }
            deriving (Eq)


-- | The reason for staling pipeline states.
data Reason = Structural | Data deriving (Eq,Show)

-- | Possible task states
data TaskState = Ready Task
                | Fetched Task Stubs
                | Decoded Task Stubs
                | Stalled Reason Task Stubs
                | Executed Task Stubs
                | Done Task
                deriving (Eq)

instance (Eq a, Ord a, Cost a, Show a) => Ord (PState a) where
  compare a b
    = compare (simtime a, maxcycles (coords a)) (simtime b, maxcycles (coords b))



