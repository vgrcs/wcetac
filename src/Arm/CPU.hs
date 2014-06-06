-----------------------------------------------------------------------------
--
-- Module      :  Arm.CPU
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Arm.CPU where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Word
import Data.Maybe
import Data.Number.PartialOrd
import qualified Control.Monad as Control
import System.IO.Unsafe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Memory
import Arm.Register
import Arm.RegisterName
import Arm.Pipeline hiding (update)
import Analyzer.Lattice
import Analyzer.Certificate
import Analyzer.ValueAbstraction
import Analyzer.Label


-- | The CPU domain has the shared resources Memory and Bus and a set of Cores,
--   each one with a register file and a 5 stage pipeline
data CPU a
  = CPU { memory  :: SharedMemory,
          multi :: MultiCore a,
          active :: (Int, Int),
          context :: [Context],
          stable :: Bool,
          parallel :: Bool } | BottomCPU



-- | The set of multicores
type MultiCore a = Map.Map Int (Core a)



-- |
instance (Show a, Show (PState a)) => Show (CPU a) where
  show cpu
    = "cpu= " ++ show (pipeline ((multi cpu) Map.! 0))

instance (Show a, Show (PState a)) => Show (Core a) where
  show a = show $ pipeline a


-- | Compare the shared memory plus the internals of each core
instance (Eq a,  Cost a) => Eq (CPU a) where
  BottomCPU == BottomCPU = True
  BottomCPU == b = False
  a == BottomCPU = False
  a == b  = datam (memory a) == datam (memory b) &&
           multi a == multi b


-- | Partial order on the complete CPU domain
instance (Show a, Ord a, Eq a, Ord (Core a),  Cost a) => Ord (CPU a) where
  compare a b =  let  m = memory a `compare` memory b
                      r = Map.intersectionWith compare (multi a) (multi b)
                 in if Map.size (multi a) /= Map.size (multi b)
                       then GT
                       else maximum (Map.elems r)


-- | Lattice implementation for CPU
instance (Show a, Ord a, Eq a, Cost a, Ord (PState a), Show (PState a)) => Lattice (CPU a) where
  bottom = let mem =  bottom
               main = Map.singleton 0 bottom
           in (CPU { memory = mem, multi = main,  active = (0,0), context = [],
                     stable = False, parallel = False })

  join a b
    = let mem  = join (memory a) (memory b)
          cores = Map.unionWith join (multi a) (multi b)
          stable' = stable a || stable b
          parallel' = parallel a || parallel b
          act = active b --if (active a) == (0,0) && (active b)  == (0,0) then (0,0) else (0,1)
          ctx = context b
          new = CPU { memory = mem, multi = cores,  active = act, context = ctx,
                      stable = stable', parallel = parallel' }
      in new



-- | Bus delays are defined as interval abstractions
data Bus = Bus (Int,Int) deriving (Eq)

-- |
instance Show Bus where
  show (Bus i) = show i

-- |
instance Enum Bus where
  fromEnum (Bus (a, b)) = b
  toEnum i = Bus (i, i)

-- | Simple interval arithmetics
instance Lattice Bus where
  bottom = Bus (0,0)
  join (Bus (a,b)) (Bus (x,y)) = Bus (min a x, max b y)


-- | Each Core is composed by a private register file, a pipeline and an instruction memory
data Core a = Core { registers :: Registers, pipeline :: Pipeline a, coreId :: Integer, instrMem :: Memory }

-- | Lattice implementation for Core
instance (Show a, Ord a, Eq a, Cost a, Ord (PState a), Show (PState a)) => Lattice (Core a) where
  bottom = let regs = bottom
               pipe = bottom
               mem = bottom
           in Core { registers = regs, pipeline = pipe, coreId = 0, instrMem = mem }
  join a b
    = let regs = join (registers a) (registers b)
          pipe = join (pipeline a) (pipeline b)
          cpu  = coreId a
          mem = join (instrMem a) (instrMem b)
      in Core { registers = regs, pipeline = pipe, coreId = cpu, instrMem = mem }


-- | Partial Order on Core
instance (Show a, Ord a, Eq a, Ord (PState a),  Cost a) => Ord (Core a) where
  compare a b =  let r = compare (registers a) (registers b)
                     p = compare (pipeline a) (pipeline b)
                     m = compare (instrMem a) (instrMem b)
                 in compare p r

-- |
instance (Eq a,  Cost a) => Eq (Core a) where
  a == b  = registers a == registers b
           && List.nub (pipeline a) == List.nub (pipeline b)





infeasiblePath
  :: Registers
  ->  Bool

infeasiblePath r
  = case  getReg r CPSR of
          CtrVal status@Control { control = cpsr } ->
                 if getControlI cpsr == 1
                    then True else False
          _ -> False


setCompl
   :: (CPU a)
   -> IO (CPU a)
setCompl cpu@CPU {multi, active = (parent, child) }
   = do
     let core = multi Map.! child
         regs = registers core
         CtrVal status@Control { control = cpsr } = getReg regs CPSR
         status' = status { control = setControlC cpsr }
         r' = setReg regs CPSR (CtrVal status')
         core' = core {registers = r'}
         multi' = Map.insert child core' multi
     return $ cpu {multi = multi'}

clearCompl
   :: (CPU a)
   ->  IO (CPU a)
clearCompl cpu@CPU {multi, active = (parent, child) }
   = do
     let core = multi Map.! child
         regs = registers core
     case getReg regs CPSR of
              CtrVal status@Control { control = cpsr } ->
                     let  status' = status { control = clearControlC cpsr }
                          r' = setReg regs CPSR (CtrVal status')
                          core' = core {registers = r'}
                          multi' = Map.insert child core' multi
                     in return $ cpu {multi = multi'}
              _ -> return cpu


setBreak
   :: (CPU a)
   ->  (CPU a)
setBreak cpu@CPU {multi, active = (parent, child) }
   = let core = multi Map.! child
         regs = registers core
         CtrVal status@Control { control = cpsr } = getReg regs CPSR
         status' = status { control = setControlI cpsr }
         r' = setReg regs CPSR (CtrVal status')
         core' = core {registers = r'}
         multi' = Map.insert child core' multi
     in cpu {multi = multi'}


getRegisters
  :: CPU a
  ->  Registers

getRegisters cpu@CPU {multi, active = (parent, child) }
  = let core = multi Map.! child
    in  registers core


