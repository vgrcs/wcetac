 -----------------------------------------------------------------------------
--
-- Module      :  Analyzer.PipelineModel
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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Analyzer.PipelineModel ( WCET (..), FiveStagePipeline (..),
  blocked, operators, step, isDone, merge, keep, alreadyInPipe, branchInstr, removeStubs,
  lrServerOff
) where


-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Pipeline
import Arm.Register
import Arm.Memory
import Arm.Register
import Arm.RegisterName
import Arm.Instruction
import Arm.Decoder
import Arm.Operand
import Arm.CPU
import Analyzer.LRU
import Analyzer.Lattice
import Analyzer.Stack
import Analyzer.Certificate
import Analyzer.ScreenPrinter
import Analyzer.Label
import Analyzer.ValueAbstraction
import Analyzer.Semantics hiding (apply, update)

--Ord-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Word
import Data.Bits
import Data.Vec
import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Number.PartialOrd
import System.IO.Unsafe
import Data.IORef
import Control.Monad.State hiding (lift,join)


-- | The transition functions for each pipeline stage
class (Cost t) => FiveStagePipeline t where
  fetchInstr ::  t -> Bool -> Task -> IO (AbsTaskState t)
  decodeInstr :: t -> Task -> Stubs -> IO (AbsTaskState t)
  executeInstr :: t -> Bool -> Task -> Stubs -> IO (AbsTaskState t)
  memoryInstr :: t -> Bool -> Task -> Stubs -> IO (AbsTaskState t)


-- | The Cost type is the WCET
data WCET = WCET  { taskcycles :: Int,
                    arrival :: Maybe Double,
                    finish :: Maybe Double,
                    delay :: Int,
                    cpu :: Int,
                    busy :: Bool }
            deriving (Eq)

-- |
instance Show WCET where
  show w  =  show ((relative w))
  showsPrec 0 w = ((show (relative w)) ++)
  showsPrec 1 w = ((show (cpu w)) ++)

-- |
instance Ord WCET where
  --compare w1 w2 = compare (arrival w1) (arrival w2)
  compare w1 w2 = compare (taskcycles w1) (taskcycles w2)

-- |
{-# NOINLINE lrServerOff #-}
lrServerOff :: IORef Bool
lrServerOff = unsafePerformIO $ newIORef False

tdma = 2
slots = 1
rate = 0.5
latency = 1

lr = False

-- | The timing model is currently independent from the instruction.
--   When upon a miss, export that to the caller using the adjoined boolean.
instance Cost WCET where
  emptyCost = WCET { taskcycles = 0, cpu = 0, arrival = Nothing, finish = Nothing, delay = 0, busy = False }

  start c t
    = WCET { taskcycles = 0, cpu = c, arrival = Nothing, finish = Just 0, delay = 0, busy = False}

  reset t b w
    = w { arrival = Just (fromIntegral t), taskcycles = 0 , finish = Just b }

  flushed t w
    = w { arrival = Just (fromIntegral t), taskcycles = 0, delay = 0 }

  update t b w
    = w { arrival = Just (fromIntegral t), finish = Just b }

  infeasible w
    =  w -- { taskcycles = 0 }

  fetchFailed w
    = w { taskcycles = 0 }

  sharedAccess w@WCET { taskcycles = cycles, arrival = Just ta, cpu = c, finish = Just f, delay = d}
    = do
      usingTDM <- readIORef lrServerOff
      (tf, b) <- if not usingTDM --lr
                 then do
                      let ts =  max (ta + latency) f
                          busy = f <= ta + latency
                          tf = ts + (1/rate)
                          delay = if busy then 1/rate else tf - ta
                      return (delay, busy)

                else do
                     let ts =  (round ta) `mod` tdma
                         start = slots * (c-1)
                         end = start + slots - 1
                         tf =  if  start <= ts && ts <= end
                                   then 0
                                   else if ts < start
                                           then start - ts
                                           else tdma - ts + start
                     return (fromIntegral (tf+1), False)

      return w { taskcycles = cycles + round (tf), finish = Just (ta + tf),
                 delay = (round tf), busy = b }

  prevSharedAccess w
    = finish w

  fetchedInstr w@WCET { taskcycles = cycles,
  arrival = t }
    = w { taskcycles = succ cycles } -- , finish = Nothing}

  structHazard w@WCET { taskcycles = cycles }
    = w { taskcycles = succ cycles  }

  decodedOps w@WCET { taskcycles = cycles }
    = w { taskcycles = succ cycles  }

  dataHazard w@WCET { taskcycles = cycles}
    = w { taskcycles = succ cycles }

  executedALU w@WCET { taskcycles = cycles }
    = w { taskcycles = succ cycles  }

  memoryExchange w@WCET { taskcycles = cycles }
    = w { taskcycles = succ cycles  }

  writeBack w@WCET { taskcycles = cycles }
    = w { taskcycles = succ cycles  }

  constantBound bound w@WCET { taskcycles = cycles }
    =  w { taskcycles = cycles + bound }

  relative c = taskcycles c
  --absolute c = arrival c
  busdelay c = delay c
  isBusy c = busy c




--instance (Eq a) => Ord (PState a) where
--  compare a b = compare (simtime a) (simtime b)

-- | Get all the operands associated to an instruction
operators
  :: Instruction
  -> [Operand]

operators (And op1 op2 op3) = List.nub [op1,op2, op3]
operators (Orr op1 op2 op3) = List.nub [op1,op2, op3]
operators (Eor op1 op2 op3) = List.nub [op1,op2, op3]
operators (Add op1 op2 op3) = List.nub [op1,op2, op3]
operators (Sub op1 op2 op3) = List.nub [op1,op2, op3]
operators (Rsb op1 op2 op3) = List.nub [op1,op2, op3]
operators (Mul op1 op2 op3) = List.nub [op1,op2, op3]
operators (SMul op1 op2 op3 op4) = List.nub [op1,op2, op3, op4]
operators (Mov op1 (Con con)) = [op1]
operators (Movne op1 (Con con)) = [op1]
operators (Movls op1 (Con con)) = [op1]
operators (Moveq op1 (Con con)) = [op1]
operators (Movhi op1 (Con con)) = [op1]
operators (Movge op1 (Reg r)) = [op1, (Reg r)]
operators (Mov op1 (Reg r)) = [op1, (Reg r)]
operators (Mov op1 (ArithShiftL r _)) = [op1, (Reg r)]
operators (Mov op1 (ArithShiftR r _)) = [op1, (Reg r)]
operators (Mov op1 (ArithRegShiftL r1 r2)) = [op1, (Reg r1), (Reg r2)]
operators (Mov op1 (ArithRegShiftR r1 r2)) = [op1, (Reg r1), (Reg r2)]
operators (Mov op1 (BinShiftL r _)) = [op1, (Reg r)]
operators (Mov op1 (BinShiftR r _)) = [op1, (Reg r)]
operators (Mvn op1 (Con con)) = [op1]
operators (Mvn op1 (Reg r)) = [op1, (Reg r)]
operators (Beq _) = [Reg CPSR]
operators (Bne _) = [Reg CPSR]
operators (Bgt _) = [Reg CPSR]
operators (Bge _) = [Reg CPSR]
operators (Ble _) = [Reg CPSR]
operators (Blt _) = [Reg CPSR]
operators (Blo _) = [Reg CPSR]
operators (Bls _) = [Reg CPSR]
operators (Bhi _) = [Reg CPSR]
operators (Bhs _) = [Reg CPSR]
operators (B _) = []
operators (Bl _) = []
operators (Bx op1) = [op1]
operators (Cmp op1 op2) = List.nub [op1, op2, Reg CPSR]
operators (Str (Reg reg1) op2) = [Reg reg1, op2]
operators (Strb (Reg reg1) op2) = [Reg reg1, op2]
operators (Strh (Reg reg1) op2) = [Reg reg1, op2]
operators (Swi _) = []
operators (Ldr (Reg reg1) op2) = [Reg reg1, op2]
operators (Ldrb (Reg reg1) op2) = [Reg reg1, op2]
operators (Ldrh (Reg reg1) op2) = [Reg reg1, op2]
operators (Ldrsh (Reg reg1) op2) = [Reg reg1, op2]
operators (Stmfd op1 (Mrg regList)) = [op1, Mrg regList]
operators (Stmia op1 (Mrg regList)) = [op1, Mrg regList]
operators (Ldmfd op1 (Mrg regList)) = [op1, Mrg regList]

operators (Nop) = []
operators (Printf _) = []
operators (PThreadCreate _) = []
operators (PThreadExit _) = []
operators (Exit _) = []
operators (PthreadMutexLock  _) = []
operators (PthreadCondWait  _) = []
operators (PthreadMutexUnlock  _) = []
operators (PthreadMutexDestroy  _) = []
operators (PthreadCondSignal  _) = []
operators (PthreadMutexattrInit  _) = []
operators (PthreadMutexattrSetpshared  _) = []
operators (ShmOpen  _) = []
operators (ShmUnlink  _) = []
operators (Ftruncate  _) = []
operators (Mmap  _) = []
operators (Munmap  _) = []
operators (PthreadMutexInit  _) = []
operators (PthreadMutexattrDestroy   _) = []
operators (Fork  _) = []
operators (Waitpid  _) = []
operators (Generic  _) = [Reg R13, Reg R11]
operators other = error $ "operators= " ++ (show other)


-- | Update the control word with a set of operands to be blocked
block
  :: [Operand]
  ->  Word32
  ->  Word32

block bs cpsr
   = let f control op =
           case  op of
                 Reg r -> cpsrSetByName r control
                 Con c -> control
                 Ind r -> cpsrSetByName r control
                 Bas r _ -> cpsrSetByName r control
                 Mrg regList -> List.foldl (\cpsr_ r -> cpsrSetByName r cpsr_) control regList
                 Aut r -> block [r] cpsr
                 BasShift r1 (ArithShiftL r2 _) -> List.foldl (\cpsr_ r -> cpsrSetByName r cpsr_) control [r1,r2]
                 BasShift r1 (ArithShiftR r2 _) -> List.foldl (\cpsr_ r -> cpsrSetByName r cpsr_) control [r1,r2]
                 BasShift r1 (Reg r2) -> List.foldl (\cpsr_ r -> cpsrSetByName r cpsr_) control [r1,r2]
     in List.foldl f cpsr bs


-- | Update the control word with a set of operands to be unblocked
unblock
   ::  [Operand]
       -> Word32
       -> Word32

unblock bs cpsr
   = let f control op =
           case  op of
                 Reg r -> cpsrClearByName r control
                 Con c -> control
                 Ind r -> cpsrClearByName r control
                 Bas r _ -> cpsrClearByName r control
                 Mrg regList -> List.foldl (\cpsr_ r -> cpsrClearByName r cpsr_) control regList
                 Aut r -> unblock [r] cpsr
                 BasShift r1 (ArithShiftL r2 _) -> List.foldl (\cpsr_ r -> cpsrClearByName r cpsr_) control [r1,r2]
                 BasShift r1 (ArithShiftR r2 _) -> List.foldl (\cpsr_ r -> cpsrClearByName r cpsr_) control [r1,r2]
                 BasShift r1 (Reg r2) -> List.foldl (\cpsr_ r -> cpsrClearByName r cpsr_) control [r1,r2]
     in List.foldl f cpsr bs


arrayOfBlocked
  :: Word32
  -> [RegisterName]

arrayOfBlocked cpsr
  = let  regs = listArray (R0, CPSR) (repeat Bottom)
         (names,_) = unzip (assocs regs)
         blocked r = if fromIntegral (cpsrGetByName r cpsr) == 1 then (r, True) else (r, False)
         isBlocked = List.map blocked names
         bs = List.filter (\(r,val) -> val == True) isBlocked
    in List.map (\(r,val) -> r) bs


-- | Decide if a given Operand is blocked in the control word
blocked
  ::  Word32
  -> Operand
  -> Bool

blocked cpsr (Reg reg)
   = if  fromIntegral (cpsrGetByName reg cpsr) == 1
         then True
         else False

blocked cpsr (Ind reg)
   = if  fromIntegral (cpsrGetByName reg cpsr) == 1
         then True
         else False

blocked cpsr (Con con)
   = False

blocked cpsr (Bas reg off)
   = if  fromIntegral (cpsrGetByName reg cpsr) == 1
         then True
         else False

blocked cpsr (Mrg regList)
   = let f reg = if  fromIntegral (cpsrGetByName reg cpsr) == 1
                     then True
                     else False
     in List.or (List.map f regList)

blocked cpsr (Aut op)
   = blocked cpsr op

blocked cpsr (BasShift r1 (Reg r2))
   = let f reg = if  fromIntegral (cpsrGetByName reg cpsr) == 1
                     then True
                     else False
     in List.or (List.map f [r1,r2])

blocked cpsr (BasShift r1 (ArithShiftL r2 _))
   = let f reg = if  fromIntegral (cpsrGetByName reg cpsr) == 1
                     then True
                     else False
     in List.or (List.map f [r1,r2])

blocked cpsr (BasShift r1 (ArithShiftR r2 _))
   = let f reg = if  fromIntegral (cpsrGetByName reg cpsr) == 1
                     then True
                     else False
     in List.or (List.map f [r1,r2])


done
  :: Vec3 (AbsTaskState a)
  -> Vec3 Bool

done vec
  = let  f at@AbsTaskState { task = Done _  } = True
         f _ = False
    in Data.Vec.map f vec


isProcessing
  :: Instruction
  -> Targets
  -> (AbsTaskState a)
  -> Bool

isProcessing i targets at@AbsTaskState { task }
  = processing task == i


waiting
  :: Vec3 (AbsTaskState a)
  -> [Instruction]

waiting vec
  = let e = Data.Vec.zipWith (\t d -> (t, not d)) vec (done vec)
        d = List.filter (snd) (toList e)
        g = List.map (task . fst) d
    in List.map processing g


keep :: PState a -> (PState a)
keep p@PState { coords = Coord vec, targets }
  = let is = waiting vec
        targets' = List.filter ( \a -> List.elem (fst a) is ) targets
    in p { targets = targets' }

processing
  :: TaskState
  -> Instruction

processing t
  = case t of
         Ready t@Task { taskInstr = i } -> i
         Fetched t@Task { taskInstr = i } _ -> i
         Decoded t@Task { taskInstr = i } _ -> i
         Stalled _ t@Task { taskInstr = i } _ -> i
         Executed t@Task { taskInstr = i } _ -> i
         Done t@Task { taskInstr = i } -> i

memories  t@Task { taskMemory = m, taskShared = sm } = (m, sm)


during
  :: Stage
  -> Vec3 (AbsTaskState a)
  -> Vec3 Bool

during s vec
   = let f = \stage at@AbsTaskState { stage = s' } -> if  stage == s'
                                                          then True
                                                          else False
     in Data.Vec.map (f s) vec

isDone
  ::  (Cost a) => Instruction
      -> (PState a)
      -> IO (Maybe Task)

isDone instr s@PState { mem, shared, coords = Coord state }
  = let  s1 = getElem 0 state
         s2 = getElem 1 state
         s3 = getElem 2 state
         check at@AbsTaskState { stage = WB, task } = if processing task == instr then True else False
         check _ = False
         multiple = List.length (List.filter (\b -> b == True) (Data.Vec.toList (during WB state))) >1
    in if  check s1  && not (check s2) && not (check s3) && not multiple
           then let at@AbsTaskState { property = cycles, stage = WB, task = Done s} = s1
                    shared' = join  shared (taskShared s)
                    mem' = join  mem (taskMemory s)
                    s' = s { taskMemory = mem', taskShared = shared'}
                in return $ Just s'
           else if  check s2 && not (check s1) && not (check s3) && not multiple
                     then let at@AbsTaskState { property = cycles, stage = WB, task = Done s} = s2
                              shared' = join  shared (taskShared s)
                              mem' = join  mem (taskMemory s)
                              s' = s { taskMemory = mem', taskShared = shared'}
                          in return $ Just s'
                     else if  check s3 &&  not (check s1) &&  not (check s2) && not multiple
                              then do
                                   let at@AbsTaskState {property = cycles, stage = WB, task = Done s } = s3
                                       shared' = join  shared (taskShared s)
                                       mem' = join  mem (taskMemory s)
                                       s' = s { taskMemory = mem', taskShared = shared'}
                                   return $ Just s'
                              else return $ Nothing


step
  :: (Show a, Cost a, FiveStagePipeline a) => Label
  -> Instruction
  -> (PState a)
  -> IO (PState a)

step label i state@PState { targets = t }
  =  do
     d <- next state
     e <- apply state d
     g <- flush i (timeToContext e)
     f <- cacheToContext g
     a <- cpsrToContext f
     b <- pcToContext i a
     c <- fileToContext b
     return c

apply
  :: (Cost a) => (PState a)
  -> (FunArray a)
  -> IO (PState a)

apply s@PState { simtime = t, nextpc, cpsr, regfile, mem, shared, coords = Coord state }  fun
  =  do let state' =  Data.Vec.zipWith
                      (\f at@AbsTaskState { property = cycles, task = s} -> f cycles s)
                      fun state
        seq <- sequence (Data.Vec.toList state')
        let state'' =  Data.Vec.fromList seq
            s' = s { coords = Coord state'' }
        return s'

alreadyInPipe instrs s@PState { coords = Coord column, targets }
  =  let flags i = Data.Vec.toList $ Data.Vec.map (isProcessing i targets) column
         all = List.concat $ List.map flags instrs
     in List.any (== True) all

branchInPipe s@PState { coords = Coord column, targets }
  =  let f at@AbsTaskState { task } = branchInstr (processing task)
     in or $ Data.Vec.toList $ Data.Vec.map f column


clearInfeasible s@PState { coords = Coord column }
  = let c stub = if  Map.member CPSR stub
                     then case getRegStub stub CPSR of
                               CtrVal s ->  let clear = s { control = clearControlI (control s) }
                                           in setRegStub stub CPSR (CtrVal clear)
                               Bottom -> stub
                     else stub
        r file = case getReg file CPSR of
                      CtrVal s ->  let clear = s { control = clearControlI (control s) }
                                  in setReg file CPSR (CtrVal clear)
                      Bottom -> file
        f at@AbsTaskState { task } =
             case task of
                  Ready t -> at
                  Fetched t@Task {taskRegisters = file } stub ->
                            at { task = Fetched t {taskRegisters = r file } (c stub) }
                  Decoded t@Task {taskRegisters = file } stub ->
                            at { task = Decoded t {taskRegisters = r file }  (c stub) }
                  Stalled reason t@Task {taskRegisters = file } stub ->
                            at { task = Stalled reason t {taskRegisters = r file } (c stub) }
                  Executed t@Task { taskRegisters = file } stub ->
                            at { task = Executed t { taskRegisters = r file } (c stub) }
                  Done t@Task { taskRegisters = file} ->
                            at { task = Done t {taskRegisters = r file}  }
     in s { coords = Coord (Data.Vec.map f column) }


merge
  :: (Cost a, Eq a, Ord a, Show a) =>
  StateT (SimContext a) IO (PState a)

merge
   = do
     (i:is) <- gets simInstrs
     cpu@CPU{ memory, multi, active = (parent, child), stable, parallel } <- gets simCPU
     let core@Core {registers, pipeline = pipe, instrMem}
            = if   Map.member child multi
                   then multi Map.! child
                   else error $ "child not found <Merge> " ++ show child ++ "\n"

     let  localpipe = filter (alreadyInPipe (i:is)) pipe
          branchs = filter branchInPipe pipe

          updated = List.map merge (List.sort (List.nub (localpipe ++ branchs)))

          merge s@PState{ mem = m}
            =  let s' = clearInfeasible s
               in  s' { regfile = registers, shared = memory,
                        stableP = stable, parallelP = parallel }
     case pipe of
          p:[] -> return $ (merge p)
          _ -> if  List.null updated
                   then  return $ (merge (List.last pipe))
                   else  --if alternative
                         --   then  return (keep, states)
                         --   else
                         return $ (List.maximum updated)



flush
 :: (Cost a) => Instruction
 -> (PState a)
 -> IO (PState a)

flush i s@PState { simtime = t, busytime = b, nextpc, cpsr, regfile = file, mem, coords = Coord state}
 = do
   let  ready s@PState { nextpc, regfile} (m,sm)
           = Ready Task { taskInstr = Nop, taskNextPc = nextpc, taskCpsr =  0,
                          taskRegisters = regfile, taskMemory = m, taskShared = sm }

        f at@AbsTaskState { property = cycles, stage = WB, task = Done task }
             = if  i == processing (Done task)
                   then at { task = Done task }
                   else at { property = flushed t cycles , stage = FI, task = ready s (memories task) }

        f at@AbsTaskState { property = cycles, stage = DI, task = Fetched task stub }
             = if  i == processing (Fetched task stub)
                   then at { task = Fetched task stub }
                   else at { property = flushed t cycles, stage = FI, task = ready s (memories task) }

        f at@AbsTaskState { property = cycles, task = Stalled r task stub }
             = if  i == processing (Stalled r task stub)
                   then at { stage = DI, task = Stalled r task stub }
                   else at { property = flushed t cycles, stage = FI, task = ready s (memories task) }

        f at@AbsTaskState { property = cycles, stage = EX, task = Decoded task stub }
             = if  i == processing (Decoded task stub)
                   then at { task = Decoded task stub }
                   else at { property = flushed t cycles, stage = FI, task = ready s (memories task) }

        f at@AbsTaskState { property = cycles, stage = MEM, task = Executed task stub }
             = if  i == processing (Executed task stub)
                   then at { task =  Executed task stub }
                   else at { property = flushed t cycles, stage = FI, task = ready s (memories task) }
        f node = node

        getNextPc pc at@AbsTaskState { task = state' }
           = if i == processing state'
                then case List.lookup i (targets s) of
                          Just p -> p
                          Nothing -> pc
                else pc
        nextpc' = Data.Vec.foldl getNextPc nextpc state
        vec' = Data.Vec.map f state
   if branchInstr i
      then  return s { nextpc = nextpc', cpsr = 0, coords = Coord vec'}
      else  return s

removeStubs :: (Cost a) => Bool -> (PState a) ->  PState a
removeStubs True a@PState{ coords = Coord c }
   = a { regfile = bottom, shared = bottom }
removeStubs False a@PState{ coords = Coord c }
   = a { regfile = bottom, shared = bottom, coords = Coord (Data.Vec.map cleanAbsTaskState c) }

cleanTask tk@Task {} = tk {taskRegisters = bottom :: Registers, taskShared = bottom :: SharedMemory }
cleanTaskState (Ready t) = Ready (cleanTask t)
cleanTaskState (Fetched t s) = Fetched (cleanTask t) s
cleanTaskState (Decoded t s) = Decoded (cleanTask t) s
cleanTaskState (Stalled r t s) = Stalled r (cleanTask t) s
cleanTaskState (Executed t s) = Executed (cleanTask t) s
cleanTaskState (Done t) = Done (cleanTask t)
cleanAbsTaskState t@AbsTaskState { task = x } = t {task = cleanTaskState x}


branchInstr (Beq _) = True
branchInstr (Bne _) = True
branchInstr (Bgt _) = True
branchInstr (Bge _) = True
branchInstr (Ble _) = True
branchInstr (Blt _) = True
branchInstr (Blo _) = True
branchInstr (Bls _) = True
branchInstr (Bhi _) = True
branchInstr (Bhs _) = True
branchInstr (B _) = True
branchInstr (Bl _) = True
branchInstr (Ldmfd _ _) = True
branchInstr _ = False


timeToContext
 :: (Cost a ) => (PState a)
 -> (PState a)

timeToContext s@PState { simtime = t, coords = Coord state, busytime, busyPeriods = b }
 = let  f acc at@AbsTaskState { property = p }
              = (prevSharedAccess p):acc

        z acc at@AbsTaskState { property = p, stage = FI, task = Stalled Structural _ _}
              = (round (fromJust (prevSharedAccess p))):acc
        z acc at =(succ t):acc


        g (acc, t) at@AbsTaskState { stage = FI, task = Stalled Structural _ _, property = p }
                      = if isBusy p then (succ acc, succ t) else (acc, succ t)
        g acc at = acc

        h acc at@AbsTaskState { stage = FI, task = Stalled Structural _ _, property = p }
              = (round . fromJust . prevSharedAccess) p
        h acc at = succ acc

        arrival' = List.maximum $ Data.Vec.foldl z [] state

        busy = case List.maximum $ Data.Vec.foldl f [] state of
                    Just b -> b

        periods = Data.Vec.foldl g b state

   in s { busytime = busy, busyPeriods =  periods, simtime = arrival'}




fileToContext
 :: (PState a)
 -> IO (PState a)

fileToContext s@PState {regfile, shared, coords = Coord state }
 = do
   let  f (r, s) at@AbsTaskState { stage = WB, task = Done task }
          = let file' = join r (taskRegisters task)
                sm' = join s (taskShared task)
            in (file', sm')
        f (r, s) _  = (r, s)
        (file', sm') = Data.Vec.foldl f (regfile, shared) state
   return s { regfile = file', shared = sm' }



cacheToContext
 :: (Cost a) => (PState a)
 -> IO (PState a)

cacheToContext  s@PState { mem, coords = Coord state }
 = do
   let f (m,t) at@AbsTaskState { property = p, stage = FI, task = Fetched task _  }
          = let t' = relative p
            in if  t' > t
                   then (taskMemory task, t')
                   else (m, t')

       f (m, t) at@AbsTaskState { property = p, stage = FI, task = Stalled Structural task _  }
          = let t' = relative p
            in if  t' > t
                   then (taskMemory task, t')
                   else (m, t')
       f (m, t) _ = (m, t)

       (mem', t') = Data.Vec.foldl f (mem, 0) state
   return s { mem = mem' }




targetPcFromContext
  :: Word32
  -> Instruction
  -> Word32
  -> Targets
  -> (Word32, Targets)

targetPcFromContext nextpc instr taskPc targets
  =  case List.lookup instr targets of
          Just pc -> if taskPc == pc
                       then (max nextpc pc, targets)
                       else if taskPc > pc && not (branchInstr instr)
                            then let t' = List.filter (\a -> fst a /= instr ) targets
                                     t'' = List.nub $ t' ++ (instr, taskPc):[]
                                 in (taskPc, t'')
                       else (max nextpc pc, targets)
          Nothing -> let (is, pcs) = unzip targets
                         pc' = 4 + List.maximum pcs
                         targets' = List.nub $ targets ++ (instr, pc'):[]
                    in (max nextpc pc', targets')


pcToContext
  :: Instruction
  -> (PState a)
  -> IO (PState a)

pcToContext  _ s@PState {nextpc,  coords = Coord state, targets = tgt }
  = do
    let  update (p, tgt) at@AbsTaskState { task =  Fetched t stub }
                =  targetPcFromContext p (taskInstr t) (taskNextPc t) tgt
         update (p, tgt) at@AbsTaskState { stage =  FI, task = Stalled _ t stub }
                =  targetPcFromContext p (taskInstr t) (taskNextPc t) tgt
         update p _ = p
         (pcFetch, targets') = Data.Vec.foldl update (nextpc, tgt) state
    return s { nextpc = pcFetch, targets = targets' }

cpsrToContext
  :: (PState a)
  -> IO (PState a)

cpsrToContext s@PState { cpsr, coords = Coord state }
  = do
    let  update (p,c) at@AbsTaskState { task }
            =  case task of
                    Decoded t@Task { taskInstr = i } stub ->
                       (p, block (operators i) c)
                    Done t@Task { taskInstr = i, taskRegisters = file } ->
                       (p, unblock (operators i) c)
                    _ -> (p,c)
         (_,cpsr') = Data.Vec.foldl update (-1,cpsr) state
         s' = s { cpsr = cpsr' }
    return s'


next
  :: (FiveStagePipeline a, Cost a) =>
  (PState a)
  -> IO (FunArray a)

next state@PState { targets }
  = do
    let  Coord vec = coords state
         hazard1 = stalled targets FI vec
         hazard2 = stalled targets DI vec
         hazard3 = stalled targets EX vec
         hazard' = Data.Vec.zipWith (||) hazard1 hazard2
         hazard'' = Data.Vec.zipWith (||) hazard' hazard3
         hazard_ = priority targets (nextpc state) vec
         priorities = Data.Vec.zipWith (||) hazard'' hazard_

    let fun = Data.Vec.map (regular state) priorities
    return $ Data.Vec.zipWith (\f abst -> f (stage abst)) fun vec


stalled
  :: Targets
  -> Stage
  -> Vec3 (AbsTaskState a)
  -> Vec3 Bool

stalled targets s vec
   = let  f t@AbsTaskState { stage = my_stage, task } =
               case task of
                    (Stalled r _ _) ->  s == my_stage
                    (Fetched _ _) -> s == my_stage
                    (Decoded _ _) -> s == my_stage
                    (Executed _ _) -> s == my_stage
                    _ ->  False
          vec' = Data.Vec.map f vec
          r0 = getElem 0 vec'
          r1 = getElem 1 vec'
          r2 = getElem 2 vec'
          pc = Data.Vec.map (getPC targets) vec
          true = True
          false = False
     in case (r0, r1, r2) of
             (False,False,False) -> false :. false :. false :.()
             (False,False,True)  -> false :. false :. false :.()
             (False,True,False)  -> false :. false :. false :.()
             -- |
             (False,True,True)   -> if getElem 1 pc < getElem 2 pc
                                      then false :. false :. true :.()
                                      else false :. true :. false :.()
             (True,False,False)  -> false :. false :. false :.()
             -- |
             (True,False,True)   -> if getElem 0 pc < getElem 2 pc
                                      then false :. false :. true :.()
                                      else true :. false :. false :.()
             -- |
             (True,True,False)   -> if getElem 0 pc < getElem 1 pc
                                      then false :. true :. false :.()
                                      else true :. false :. false :.()
             -- |
             (True,True,True)    -> if getElem 0 pc < getElem 1 pc && getElem 0 pc < getElem 2 pc
                                      then false :. true :. true :.()
                                   else if getElem 1 pc < getElem 0 pc && getElem 1 pc < getElem 2 pc
                                           then true :. false :. true :.()
                                   else  true :. true :. false :.()

priority
   :: Targets
   ->  Word32
   -> Vec3 (AbsTaskState a)
   -> Vec3 Bool -- (Stage, Bool)

priority targets pc vec
   = let  f t@AbsTaskState { stage = FI, task = Ready _ } = (True, FI)
          f t@AbsTaskState { stage = WB, task = Done _ } = (True, WB)
          f _ = (False, FI)
          ready = Data.Vec.map f vec
          r0 = getElem 0 ready
          r1 = getElem 1 ready
          r2 = getElem 2 ready
          --true = (FI, True)
          --false = (FI, False)
          true = True
          false = False
     in case  (r0, r1, r2) of
              ((False,_), (False,_), (False,_)) -> false :. false :. false :.()
              ((False,_), (False,_) ,(True,_))  -> false :. false :. false :.()
              ((False,_) ,(True,_), (False,_))  -> false :. false :. false :.()
              -- |
              ((False,_), (True,FI), (True,FI))  -> false :. false :. true :.()
              ((False,_), (True,WB), (True,FI))  -> false :. false :. true :.()
              ((False,_), (True,FI), (True,WB))  -> false :. true :. false :.()

              ((True,_), (False,_), (False,_))  -> false :. false :. false :.()
              -- |
              ((True,FI), (False,_), (True,FI)) -> false :. false :. true :.()
              ((True,WB), (False,_), (True,FI)) -> false :. false :. true :.()
              ((True,FI), (False,_), (True,WB)) -> true :. false :. false :.()

              -- |
              ((True,FI), (True,FI), (False,_))   -> false :. true :. false :.()
              ((True,WB), (True,FI), (False,_))   -> false :. true :. false :.()
              ((True,FI), (True,WB), (False,_))   -> true :. false :. false :.()

              -- |
              ((True,FI), (True,FI), (True,FI))   -> false :. true :. true :.()
              ((True,FI), (True,FI), (True,WB))   -> true :. true :. false :.()
              ((True,FI), (True,WB), (True,FI))   -> true :. false :. true :.()
              ((True,WB), (True,FI), (True,FI))   -> false :. true :. true :.()




regular
  :: (FiveStagePipeline a, Cost a) => (PState a)
  -> Bool -- [(Stage, Bool)]
  -> Stage
  -> (AbsTask a)

regular vec@PState { simtime = t, nextpc, cpsr, mem, busytime = b, parallelP }  hazards FI
   = do
     \ cycles state ->
       do
       case state of
            Ready task -> do
                          if not hazards
                              then let task' = task { taskInstr = Nop, taskNextPc = nextpc, taskMemory = mem }
                                       cycles' = reset t b cycles
                                   in fetchInstr cycles' parallelP task'
                              else do
                                   return $ AbsTaskState { property = reset t b cycles, stage = FI,
                                                           task = Ready task }
            Stalled _ task s -> do
                                let i = taskInstr task
                                    cycles' = update t b cycles
                                    cpsr' = unblock (operators i) (taskCpsr task)
                                    task' = task { taskCpsr = cpsr }
                                return $ AbsTaskState { property = fetchedInstr cycles', stage = DI,
                                                        task = Fetched task' s }

regular vec@PState { simtime = t, nextpc, cpsr, regfile, mem, shared} hazards DI
   = do
     let contextData task
            = task { taskRegisters = regfile, taskShared = shared }

         stubsFromContext stub task
            = let update s n = setRegStub s n (getReg regfile n)
              in List.foldl update stub (names (taskInstr task))

         contextCpsr task
            = task  { taskCpsr = cpsr }

     \ cycles state ->
       do
       if not hazards
           then case  state of
                      Fetched task s   -> let task' = contextCpsr task
                                              stub' = stubsFromContext s task
                                         in decodeInstr cycles task' stub'

                      Stalled _ task s -> let t' = (contextCpsr . contextData ) task
                                              s' = stubsFromContext  s task
                                         in decodeInstr cycles t' s'

           else case  state of
                      Fetched task s  -> let task' = contextCpsr task
                                        in return $ AbsTaskState { property = dataHazard cycles,
                                                                   stage = DI,
                                                                   task = Stalled Data task' s }
                      Stalled r task s -> let task' = contextCpsr task
                                         in return $ AbsTaskState { property = dataHazard cycles,
                                                                    stage = DI,
                                                                    task = Stalled r task' s }

regular vec@PState { simtime = t, stableP } hazards EX
   = \ cycles (Decoded task s) ->
          if  not hazards
              then executeInstr cycles stableP task s
              else return $ AbsTaskState { property = dataHazard cycles,
                                           stage = EX, task = Stalled Structural task s }


regular vec@PState { simtime = t, nextpc, cpsr, regfile, mem, stableP } _ MEM
   = \ cycles (Executed task s) ->
        do
        let task' = task { taskRegisters = regfile}
        wb <- memoryInstr cycles stableP task' s
        return wb


regular vec@PState { simtime = t, nextpc = pc, mem = m, shared = s, targets, busytime = b,
                     parallelP } hazards WB
   = do
     \ cycles ( Done task@Task { taskInstr = i' , taskRegisters = file' } ) ->
      do let cycles' = reset t b cycles
         let task' = task { taskInstr = Nop, taskNextPc = pc, taskShared = s, taskMemory = m}
         fetchInstr cycles' parallelP task'
