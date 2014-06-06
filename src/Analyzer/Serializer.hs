-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Serializer
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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Analyzer.Serializer (

) where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Binary
import Data.Map
import Control.Monad
import qualified Data.Vec as Vec
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as ByteString
import Codec.Compression.GZip

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Memory
import Arm.Register
import Arm.RegisterName
import Arm.Pipeline
import Arm.Instruction
import Arm.Operand
import Analyzer.Stack
import Analyzer.LRU
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.ValueAbstraction
import Analyzer.Certificate
import Analyzer.PipelineModel

instance Binary Label where
   put Empty     =  putWord8 0
   put (Head l)  =  do {  put l}
   put (NodeL l) =  do {  put l}
   put (ExitL l) =  do {  put l}
   put (CallL l) =  do {  put l}
   put (RootL l) =  do {  put l}
   put (HookL l) =  do {  put l}

   get = do t <- get :: Get Word8
            case t of
                 0 -> return Empty
                 1 -> do {v <- get; return (Head v)}
                 2 -> do {v <- get; return (NodeL v)}
                 4 -> do {v <- get; return (ExitL v)}
                 5 -> do {v <- get; return (CallL v)}
                 6 -> do {v <- get; return (RootL v)}
                 7 -> do {v <- get; return (HookL v)}

instance Binary Identifier where
   put i@Identifier { labelId, procedure, ilvpos}
      = do { put labelId; put procedure; put ilvpos}

   get = do a <- get
            b <- get
            c <- get
            return Identifier {labelId=a, procedure=b, ilvpos=c}


instance Binary Proc where
   put i@Proc { procId, section, procName}
      = do { put procId; put section; put procName }

   get = do a <- get
            b <- get
            c <- get
            return Proc {procId=a, section=b, procName=c}

--instance (Binary a) => Binary (Invs a) where
--  put i = put (i :: Map Point (Node a))

instance (Binary b) => Binary (Node b) where
   put (node@Node { value = a,  redirect = b } )
       = do {put a; put b}
       -- = do {put b}

   get = do a <- get
            b <- get
            return (Node { value = a, stableFixpoint = False, stableLoop = False,
                           stableValue = [], insideLoop = No,
                           contexts = [], redirect = b, infeasibleNode = False})


--instance (Binary a) => Binary (MultiCore a) where
--   put m = put (m :: Data.Map.Map Int (Core a))


instance (Binary a) => Binary (CPU a) where
   put BottomCPU = put (0 :: Word8)
   put cpu = do put (1 :: Word8)
                put (memory cpu)
                put (multi cpu)
                put (active cpu)
                put (context cpu)
                --put (infeasibleArch cpu)
                put (parallel cpu)
                put (stable cpu)

   get = do a <- get
            b <- get
            c <- get
            d <- get
            --e <- get
            f <- get
            g <- get
            return (CPU {  memory = a , multi = b,  active = c, context = d, --infeasibleArch = e,
                           parallel = f, stable = g })

--instance Binary Cache where
--   put c = put (c :: Seq.Seq [CacheLine])

instance Binary SharedMemory where
   put m = do
           put (datam m)
           put (stack m)
   get = do
         m1 <- get
         m2 <- get
         return $ SharedMemory { datam = m1, stack = m2 }

instance (Binary a ) => Binary (Stack a) where
    put (St s) = put s
    get = do {s <- get; return (St s)}

instance Binary DataMem where
   put (DataMem s) = do put s
   get = liftM DataMem get

instance (Binary a) => Binary (Core a) where
   put core = do put $ instrMem core
                 put $ registers core
                 put $ pipeline core
                 --put $ coreId core

   get = do s <- get
            r <- get
            p <- get
            i <- get
            return (Core {instrMem = s,  registers = r, pipeline = p, coreId = i })

instance Binary Memory where
   put m
     = do put (must m)
          put (mustL1 m)
          put (mustL2 m)

   get = do m3 <- get
            m5 <- get
            m7 <- get
            return (Memory { must = m3, mustL1 = m5, mustL2 = m7})

instance Binary L2Box where
   put (L2Box l2) = put l2
   get = do {l2 <- get; return (L2Box l2)}

instance Binary L1Box where
   put (L1Box l1) = do put l1

   get = do l1 <- get
            return (L1Box l1)





instance Binary Value where
   put Bottom  =  put (0 :: Word8)
   put (RegVal v) =  do {  put v}
   put (PtrVal v) =  do {  put v}
   put (CtrVal v) =  do {  put v}
   put (StdVal v) =  do {  put v}
   put (BackVal v) =  do {  put v}
   put (CharVal v) =  do {  put v}
   put (MemVal v) =  do {  put v}
   put (ConVal v) =  do {  put v}

   get = do t <- get :: Get Word8
            case t of
                 0 -> return Bottom
                 1 -> do {v <- get; return (RegVal v)}
                 2 -> do {v <- get; return (PtrVal v)}
                 6 -> do {v <- get; return (CtrVal v)}
                 7 -> do {v <- get; return (StdVal v)}
                 8 -> do {v <- get; return (BackVal v)}
                 9 -> do {v <- get; return (CharVal v)}
                 10 -> do {v <- get; return (MemVal v)}
                 11 -> do {v <- get; return (ConVal v)}

instance Binary Control where
   put c = do
           put $ control c
           put $ lessthan c
           put $ equal c
           put $ greaterthan c
           put $ back c
           put $ lower c
           put $ higher c
           put $ lowersame c
           put $ highersame c

   get = do cpsr <- get
            lt <- get
            eq <- get
            gt <- get
            b <- get
            l <- get
            h <- get
            ls <- get
            hs <- get
            return (Control { control = cpsr,  lessthan = lt, equal = eq, greaterthan = gt, back = b,
                              lower = l, higher = h, lowersame = ls, highersame = hs })

--instance Binary Stubs where
--   put s = put (s :: Map RegisterName Value)

instance Binary Registers where
   put (Registers r) = put r
   get = do {r <- get; return (Registers r) }


instance (Binary a) => Binary (PState a) where
   put (s@PState { simtime = a, nextpc = b, cpsr = c, regfile = d,
                   mem = e, shared = f, coords = g, targets = h, final = i, busytime = l} )
       = do {put a; put b; put c; put d; put e; put f; put g; put h; put i; put l}

   get = do
         a <- get
         b <- get
         c <- get
         d <- get
         e <- get
         f <- get
         g <- get
         h <- get
         i <- get
         l <- get
         return (PState { simtime = a, nextpc = b, cpsr = c, regfile = d,
                          mem = e, shared = f, coords = g, targets = h, final = i,
                          stableP = False, busytime = l, parallelP = False, busyPeriods = (0,0) } )


instance Binary OpCode where
   put (OpCode w) = do {put (0 :: Word8); put w}
   put BottomW = put (1 :: Word8)

   get = do  t <- get :: Get Word8
             case t of
                  0 -> do {w <- get; return (OpCode w)}
                  1 -> return BottomW

instance Binary RegisterName where
   put R0   = putWord8 0
   put R1   = putWord8 1
   put R2   = putWord8 2
   put R3   = putWord8 3
   put R4   = putWord8 4
   put R5   = putWord8 5
   put R6   = putWord8 6
   put R7   = putWord8 7
   put R8   = putWord8 8
   put R9   = putWord8 9
   put R10  = putWord8 10
   put R11  = putWord8 11
   put R12  = putWord8 12
   put R13  = putWord8 13
   put R14  = putWord8 14
   put R15  = putWord8 15
   put CPSR = putWord8 16
   put SP   = putWord8 17
   put FP   = putWord8 18
   put IP   = putWord8 19
   put LR   = putWord8 20
   put PC   = putWord8 21

   get = do t <- get :: Get Word8
            case t of
                  0 -> return R0
                  1 -> return R1
                  2 -> return R2
                  3 -> return R3
                  4 -> return R4
                  5 -> return R5
                  6 -> return R6
                  7 -> return R7
                  8 -> return R8
                  9 -> return R9
                  10 -> return R10
                  11 -> return R11
                  12 -> return R12
                  13 -> return R13
                  14 -> return R14
                  15 -> return R15
                  16 -> return CPSR
                  17 -> return SP
                  18 -> return FP
                  19 -> return IP
                  20 -> return LR
                  21 -> return PC


instance (Binary a) => Binary (Coord a) where
   put (Coord vec) = put vec
   get = do {vec <- get; return (Coord vec)}


instance Binary Instruction where
   put (Add op1 op2 op3) = do {put op1; put op2; put op3}
   put (And op1 op2 op3) = do {put op1; put op2; put op3}
   put (B op) = do {put op}
   put (Beq op) = do {put op}
   put (Bgt op) = do {put op}
   put (Bic op1 op2 op3) = do {put op1; put op2; put op3}
   put (Bl op) = do {put op}
   put (Blt op) = do {put op}
   put (Bne op) = do {put op}
   put (Cmp op1 op2) = do {put op1; put op2}
   put (Eor op1 op2 op3) = do {put op1; put op2; put op3}
   put (Ldmia op1 op2) = do {put op1; put op2}
   put (Ldmfd op1 op2) = do {put op1; put op2}
   put (Ldr op1 op2) = do {put op1; put op2}
   put (Ldrb op1 op2) = do {put op1; put op2}
   put (Mov op1 op2) = do {put op1; put op2}
   put (Mul op1 op2 op3) = do {put op1; put op2; put op3}
   put (Orr op1 op2 op3) = do {put op1; put op2; put op3}
   put (Stmfd op1 op2) = do {put op1; put op2}
   put (Str op1 op2) = do {put op1; put op2}
   put (Strb  op1 op2) = do {put op1; put op2}
   put (Sub op1 op2 op3) = do {put op1; put op2; put op3}
   put (Swi op) = do {put op}
   put (PthreadMutexLock op) = do {put op}
   put (PthreadCondWait  op) = do {put op}
   put (PthreadMutexUnlock op) = do {put op}
   put (PthreadMutexDestroy op) = do {put op}
   put (PthreadCondSignal op) = do {put op}
   put (PthreadMutexattrInit op) = do {put op}
   put (PthreadMutexattrSetpshared op) = do {put op}
   put (ShmOpen op) = do {put op}
   put (ShmUnlink op) = do {put op}
   put (Ftruncate op) = do {put op}
   put (Mmap op) = do {put op}
   put (Munmap op) = do {put op}
   put (PthreadMutexInit op) = do {put op}
   put (PthreadMutexattrDestroy op) = do {put op}
   put (Fork op) = do {put op}
   put (Waitpid op) = do {put op}
   put Nop = putWord8 1 -- 40
   put (Ble op) = do {put op}
   put (Printf op) = do {put op}
   put (Strh  op1 op2) = do {put op1; put op2}
   put (Ldrh op1 op2) = do {put op1; put op2}
   put (Bge op) = do {put op}
   put (Bls op) = do {put op}
   put (Ldrsh op1 op2) = do {put op1; put op2}
   put (Mvn op1 op2) = do {put op1; put op2}
   put (Rsb op1 op2 op3) = do {put op1; put op2; put op3}
   put (Generic op1 ) = do {put op1 }
   put (Movge op1 op2) = do {put op1; put op2}
   put (Movne op1 op2) = do {put op1; put op2}
   put (Moveq op1 op2) = do {put op1; put op2}
   put (Movls op1 op2) = do {put op1; put op2}
   put (Movhi op1 op2) = do {put op1; put op2}
   put other = error $ "put? " ++ show other


   get = do t <- get :: Get Word8
            case t of
                 0 -> liftM3 Add get get get
                 1 -> liftM3 And get get get
                 2 -> liftM B get
                 3 -> liftM Beq get
                 4 -> liftM Bgt get
                 5 -> liftM3 Bic get get get
                 6 -> liftM Bl get
                 7 -> liftM Blt get
                 8 -> liftM Bne get
                 9 -> liftM2 Cmp get get
                 10 -> liftM3 Eor get get get
                 11 -> liftM2 Ldmia get get
                 12 -> liftM2 Ldmfd get get
                 13 -> liftM2 Ldr get get
                 14 -> liftM2 Ldrb get get
                 15 -> liftM2 Mov get get
                 16 -> liftM3 Mul get get get
                 17 -> liftM3 Orr get get get
                 19 -> liftM2 Stmfd get get
                 20 -> liftM2 Str get get
                 21 -> liftM2 Strb get get
                 22 -> liftM3 Sub get get get
                 23 -> liftM Swi get
                 24 -> liftM PthreadMutexLock get
                 25 -> liftM PthreadCondWait get
                 26 -> liftM PthreadMutexUnlock get
                 27 -> liftM PthreadMutexDestroy get
                 28 -> liftM PthreadCondSignal get
                 29 -> liftM PthreadMutexattrInit get
                 30 -> liftM PthreadMutexattrSetpshared get
                 31 -> liftM ShmOpen get
                 32 -> liftM ShmUnlink get
                 33 -> liftM Ftruncate get
                 34 -> liftM Mmap get
                 35 -> liftM Munmap get
                 36 -> liftM PthreadMutexInit get
                 37 -> liftM PthreadMutexattrDestroy get
                 38 -> liftM Fork get
                 39 -> liftM Waitpid get
                 40 -> return Nop
                 41 -> liftM Ble get
                 42 -> liftM Printf get


instance (Binary a) => Binary (AbsTaskState a) where
   put (a@AbsTaskState {property = i, stage = s, task = t} )
     = do put i
          put s
          put t

   get = do i <- get
            s <- get
            t <- get
            return (AbsTaskState {property = i, stage = s, task = t} )


instance (Binary a) => Binary (Vec.Vec3 a) where
   put vec = put (Vec.toList vec)
   get = get

instance Binary Stage where
  put FI  =  putWord8 0
  put DI  =  putWord8 1
  put EX  =  putWord8 2
  put MEM =  putWord8 3
  put WB  =  putWord8 4

  get = do  t <- get :: Get Word8
            case t of
                 0 -> return FI
                 1 -> return DI
                 2 -> return EX
                 3 -> return MEM
                 4 -> return WB

instance Binary TaskState where
  put (Ready t) = do {put t}
  put (Fetched t s) = do { put t; put s}
  put (Decoded t s) = do { put t; put s}
  put (Stalled r t s) = do { put r; put t; put s}
  put (Executed t s) = do { put t; put s}
  put (Done t) = do { put t}

  get = do  t <- get :: Get Word8
            case t of
                 0 -> do {t <- get; return (Ready t)}
                 1 -> do {t <- get; s <- get; return (Fetched t s)}
                 2 -> do {t <- get; s <- get; return (Decoded t s)}
                 3 -> do {r <- get; t <- get; s <- get; return (Stalled r t s)}
                 4 -> do {t <- get; s <- get; return (Executed t s)}
                 5 -> do {t <- get; return (Done t)}


instance Binary Reason where
  put Structural = put (0 :: Word8)
  put Data = put (1 :: Word8)

  get = do  t <- get :: Get Word8
            case t of
                 0 -> return Structural
                 1 -> return Data


instance Binary Operand where
  put (Aut op) = do {put op}
  put (Bas n i) = do {put n; put i}
  put (Con w) = do {put w}
  put (Ind n) = do {put n}
  put (Mrg l) = do {put l}
  put (Pos op i) = do {put op; put i}
  put (Reg n) = do {put n}
  put (Rel i) = do { put i}
  put (Lab s) = do {put s}
  put (ArithShiftL n i) = do {put n; put i}
  put (ArithShiftR n i) = do {put n; put i}
  put (BinShiftL n i) = do {put n; put i}
  put (BinShiftR n i) = do {put n; put i}
  put (BasShift n i) = do { put n; put i}
  put (ArithRegShiftL n i) = do {put n; put i}
  put (ArithRegShiftR n i) = do {put n; put i}


  get = do  t <- get :: Get Word8
            case t of
                 0 -> do {op <- get; return (Aut op)}
                 1 -> do {n <- get; i <- get; return (Bas n i)}
                 2 -> do {w <- get; return (Con w)}
                 3 -> do {n <- get; return (Ind n)}
                 4 -> do {l <- get; return (Mrg l)}
                 5 -> liftM2 Pos get get
                 6 -> liftM Reg get
                 7 -> liftM Rel get
                 8 -> liftM Lab get
                 9 -> do {n <- get; i <- get; return (ArithShiftL n i)}
                 10 -> do {n <- get; i <- get; return (ArithShiftR n i)}
                 11 -> do {n <- get; i <- get; return (BinShiftL n i)}
                 12 -> do {n <- get; i <- get; return (BinShiftR n i)}




instance Binary Task where
   put (t@Task { taskInstr = a, taskNextPc = b, taskCpsr = c, taskRegisters = d,
                 taskMemory = e, taskShared = f } )
     =  do {put a; put b; put c; put d; put e; put f}

   get = do a <- get
            b <- get
            c <- get
            d <- get
            e <- get
            f <- get
            return (Task { taskInstr = a, taskNextPc = b, taskCpsr = c, taskRegisters = d,
                           taskMemory = e, taskShared = f } )


instance Binary WCET where
   put (w@WCET { taskcycles = a, delay = b, arrival = c, finish = d, cpu = e, busy = f } )
     = do {put a; put b; put c; put d; put e; put f}

   get = do a <- get
            b <- get
            c <- get
            d <- get
            e <- get
            f <- get
            return (WCET { taskcycles = a, arrival = c, finish = d, cpu = e, delay = b, busy = f } )
