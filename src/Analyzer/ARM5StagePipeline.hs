
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ARM5StagePipeline
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Analyzer.ARM5StagePipeline where

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------

import Arm.CPU
import Arm.Pipeline
import Arm.Memory
import Arm.Register
import Arm.Decoder
import Arm.RegisterName
import Arm.Operand
import Arm.Instruction
import Arm.BinaryNumber
import Analyzer.Lattice
import Analyzer.LRU
import Analyzer.PipelineModel
import Analyzer.Stack
import Analyzer.ValueAbstraction
import Data.Number.PartialOrd
-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Word
import Data.Bits
import Data.Vec
import Data.Array
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Number.PartialOrd
import System.IO.Unsafe


-- |  Abstract the collection of concrete values to a Register domain

abstraction
  :: (Num t, Num t1) => [Word32]
  -> ((t, t1) -> Value)
  -> Value

abstraction [] t
  = Bottom

abstraction c1 t
  = let c1' = List.map toInt32 c1
        l = fromIntegral $ List.minimum c1'
        u = fromIntegral $ List.maximum c1'
     in t (l, u)

concretization Bottom = []
concretization (RegVal (l,u))
  = List.map fromIntegral [toInt32 l.. toInt32 u]

pairs2
  :: Stubs
  ->  Operand
  ->  Operand
  ->  Int
  -> IO (Value, Value)

pairs2 stub (Reg reg1) op2 0
  = do
    let  (l1,h1) = fromValueToInt32 $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (toInt32 c, toInt32 c)
                        Reg r -> fromValueToInt32 $ getRegStub stub r

         a = l1
         c = if h1 < h2 then h1 else h2-1

         b = if l1 < l2 then l2 else l1+1
         d = h2

         i1 = abstraction [fromInteger a, fromInteger c] RegVal
         i2 = abstraction [fromInteger b, fromInteger d] RegVal
    return $ if l1 > h2
                then (Bottom, Bottom)
                else (i1, i2)

pairs
  :: Stubs
  -> Operand
  -> Operand
  -> Int
  -> IO ([Word32], [Word32])


pairs stub (Reg reg1) op2 0
  = do
    let  (l1,h1) = fromValueToInt32 $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (toInt32 c, toInt32 c)
                        Reg r -> fromValueToInt32 $ getRegStub stub r

         a = l1
         c = if h1 < h2 then h1 else h2-1

         b = if l1 < l2 then l2 else l1+1
         d = h2

    return $ if l1 > h2 || a > c || b > d
                then ([], [])
                else ([fromInteger a, fromInteger c], [fromInteger b, fromInteger d])

pairs stub (Reg reg1) op2 3
  = do
    let  (l1,h1) = fromValue $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (c, c)
                        Reg r -> fromValue $ getRegStub stub r

         a = l1
         c = if h1 < h2 then h1 else h2-1

         b = if l1 < l2 then l2 else l1+1
         d = h2

    return $ if l1 > h2 || a > c || b > d
                then ([], [])
                else ([a,c], [b,d])



pairs stub (Reg reg1) op2 1
  = do
    let  (l1,h1) = fromValueToInt32 $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (toInt32 c, toInt32 c)
                        Reg r -> fromValueToInt32 $ getRegStub stub r
         l = max l1 l2
         h = min h1 h2
    if l <= h
       then return ([fromInteger l, fromInteger h], [fromInteger l, fromInteger h])
       else return ([],[])

pairs stub (Reg reg1) op2 2
  = do
    let  (l1,h1) = fromValueToInt32 $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (toInt32 c, toInt32 c)
                        Reg r -> fromValueToInt32 $ getRegStub stub r

         a = if l1 > l2 then l1 else l2+1
         c = h1

         b = l2
         d = if h1 > h2 then h2 else h1-1
    return $ if h1 < l2 || a > c || b > d
                then ([], [])
                else ([fromInteger a, fromInteger c], [fromInteger b, fromInteger d])

pairs stub (Reg reg1) op2 4
  = do
    let  (l1,h1) = fromValue $ getRegStub stub reg1
         (l2,h2) = case op2 of
                        Con c -> (c, c)
                        Reg r -> fromValue $ getRegStub stub r

         a = if l1 > l2 then l1 else l2+1
         c = h1

         b = l2
         d = if h1 > h2 then h2 else h1-1

    return $ if h1 < l2 || a > c || b > d
                then ([], [])
                else ([a, c], [b, d])


-- | Definition of backward analysis for the interval abstraction.
--   Also returns if the output predicate is the empty interval
backwardAnalysis
  :: Stubs
  -> Operand
  -> Value
  -> (Bool, Value)

backwardAnalysis _ _ Bottom  = (False, Bottom)

backwardAnalysis stub operand value
  = let value' =  case  operand of
                        Con c -> RegVal (c,c)
                        Reg r -> getRegStub stub r
    in case meet value' value of
            Bottom -> (False, Bottom)
            RegVal value'' -> (True, BackVal value'')
            MemVal value'' -> (True, MemVal value'')



compareRegs stub (Reg reg1) op2 op
  =  do
     combinations <- pairs  stub (Reg reg1) op2 op
     let i1 = abstraction (fst combinations) RegVal
         i2 = abstraction (snd combinations) RegVal

     let (left, right) =
           case op of
                0 -> ( (below i2) - (RegVal (fromInteger 1)) ,
                      (above i1) + (RegVal (fromInteger 1)) )
                3 -> ( (below i2) - (RegVal (fromInteger 1)) ,
                      (above i1) + (RegVal (fromInteger 1)) )
                1 -> (i2, i1)
                2 -> ( (above i2) + (RegVal (fromInteger 1)) ,
                      (below i1) - (RegVal (fromInteger 1)) )
                4 -> ( (above i2) + (RegVal (fromInteger 1)) ,
                      (below i1) - (RegVal (fromInteger 1)) )
     let (bool1, reg1') =  backwardAnalysis stub (Reg reg1) left
         (bool2, op2')  =  backwardAnalysis stub op2 right

     let  m = bool1 && bool2
          stub' = setRegStub Map.empty reg1 reg1'
          stub'' = case op2 of
                        Con c -> stub'
                        Reg r -> setRegStub stub' r op2'
     return (m, stub'')


backwardAnalysis2
  :: Stubs
  -> Operand
  -> Value
  -> IO Stubs
backwardAnalysis2 stub (Con c) value
  = do
    if not $ elem c (concretization value)
       then do
            case value of
                 RegVal (l,u) -> return bottom
                 _ -> return bottom
       else return stub

backwardAnalysis2 stub (Reg r) value
  = do
    let prev = getRegStub stub r
    let value' = meet prev value
    return $ setRegStub stub r value'


widening :: (Value, Value) -> Int -> (Value, Value)
widening (i1, i2) 0
  = ( (below i2) - (RegVal (fromInteger 1)) ,
      (above i1) + (RegVal (fromInteger 1)) )
widening (i1, i2) 1
  = ( i1, i2 )
widening (i1, i2) 2
  = ( (above i2) + (RegVal (fromInteger 1)) ,
      (below i1) - (RegVal (fromInteger 1)) )
widening (i1, i2) 3
  = ( (below i2) - (RegVal (fromInteger 1)) ,
      (above i1) + (RegVal (fromInteger 1)) )
widening (i1, i2) 4
  = ( (above i2) + (RegVal (fromInteger 1)) ,
      (below i1) - (RegVal (fromInteger 1)) )

compareRegs2 stub (Reg reg1) op2 op
  =  do
     combinations <- pairs  stub (Reg reg1) op2 op
     let i1 = abstraction (fst combinations) RegVal
         i2 = abstraction (snd combinations) RegVal
     let (left, right) = widening (i1,i2) op
     stub'   <-  backwardAnalysis2 (Map.delete R15 (Map.delete CPSR stub)) (Reg reg1) left
     stub''  <-  backwardAnalysis2 (Map.delete R15 (Map.delete CPSR stub)) op2 right
     let  stub''' =  meet stub' stub''
     return (meet stub' stub'')


-- | Definition of the ARM 5 stages pipeline
instance (Cost a, Show a) => (FiveStagePipeline a) where
  fetchInstr cycles parallel task@Task { taskNextPc=nextpc, taskMemory=m, taskRegisters=r }
     = do
       (cl, opcode, m') <- readMemInstrWord m nextpc
       let instr = case opcode of
                        OpCode op -> decode op
                        BottomW   -> Nothing

           infeasible = infeasiblePath r

       let  pc' = nextpc + 4
            stub' = setRegStub bottom R15 (StdVal pc')
            hit c = case  c of
                          (HR1,_) -> True
                          (HW1,_) -> error "write on read"
                          _ -> False
       case  instr of
             Just i  -> do
                       if hit cl
                          then
                            do
                            let task' = task { taskInstr = i, taskNextPc = pc', taskMemory = m' }
                            return $ AbsTaskState { property = fetchedInstr cycles, stage = DI,
                                                    task = Fetched task' stub' }
                          else
                            do
                            let task' = task { taskInstr = i, taskNextPc = pc', taskMemory = m' }
                            p' <- sharedAccess cycles
                            return $ AbsTaskState { property = p',  stage = FI,
                                                    task = Stalled Structural task' stub' }
             Nothing -> do
                        let task' = task { taskInstr = Nop }
                        return $ AbsTaskState { property = fetchFailed cycles,
                                                stage = FI,
                                                task = Ready task' }

-- | The decode transfer function detects data hazards.
-- The stubs are kept unmodified.
  decodeInstr cycles task stub
    = let  i = taskInstr task
           cpsr = taskCpsr task
           mask = List.map (blocked cpsr) (operators i)
           flag = List.foldl (||) False mask
      in if  not flag
             then return $ AbsTaskState { property = decodedOps cycles, stage = EX,
                                          task = Decoded task stub }
             else return $ AbsTaskState { property = dataHazard cycles, stage = DI,
                                          task = Stalled Data task stub }

  executeInstr cycles stable task@Task { taskRegisters = file} stub
    = case getReg file CPSR of
           CtrVal status@Control { control = cpsr } ->
                if  1 == getControlI cpsr || stable
                    then do
                         return $ AbsTaskState { property = infeasible cycles, stage = MEM,
                                                 task = Executed task stub }
                    else executeInstr_ cycles task stub
           _ -> executeInstr_ cycles task stub


  memoryInstr cycles stable task@Task { taskRegisters = file} stub
    = case getReg file CPSR of
           CtrVal status@Control { control = cpsr } ->
                if  1 == getControlI cpsr || stable
                    then do
                         return $ AbsTaskState { property = infeasible cycles, stage = WB, task = Done task }
                    else memoryInstr_ cycles task stub
           _ -> memoryInstr_ cycles task stub


-- | TODO: everything should be an interval?
executeInstr_ cycles task@Task { taskInstr = And (Reg reg1) (Reg reg2) (Con con1) } stub
  = do
    let  r2 = getRegStub stub reg2
         reg1' =  case r2 of
                      RegVal (a, b) -> RegVal (a .&. con1, b .&. con1)
                      MemVal ((a,b),_) ->  RegVal (a .&. con1, b .&. con1)
         stub' = setRegStub stub reg1 reg1'
    --putStrLn $ "and " ++ show r2 ++ " , " ++ show con1 ++ " = " ++ show reg1'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task =  Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Orr (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = do
    let  (a, b) = fromValue $ getRegStub stub reg2
         (c, d) = fromValue $ getRegStub stub reg3
         reg1' =  RegVal (a `xor` c, b .|. d)
         stub' = setRegStub stub reg1 reg1'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task =  Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Eor (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = do
    let  (a, b) = fromValue $ getRegStub stub reg2
         (c, d) = fromValue $ getRegStub stub reg3
         reg1' =  RegVal (a `xor` c, b `xor` d)
         stub' = setRegStub stub reg1 reg1'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task =  Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Add (Reg reg1) (Reg reg2) (Con con1) } stub
  = do
    let  r2 = getRegStub stub reg2
         reg1' =  case r2 of
                      RegVal _ -> r2 + (RegVal (con1, con1))
                      MemVal (v2,_) ->  RegVal (v2 + (con1, con1))
                      Bottom -> Bottom
                      other -> error $ "add r2" ++ show r2
         stub' = setRegStub stub reg1 reg1'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task =  Executed task stub' }

executeInstr_ cycles task@Task {taskInstr = Add (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = do
    let  r2 = getRegStub stub reg2
         r3 = getRegStub stub reg3
         stub' = setRegStub stub reg1 (r2 + r3)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task {taskInstr = Mul (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = do
    let  r2 = getRegStub stub reg2
         r3 = getRegStub stub reg3
         stub' = setRegStub stub reg1 (r2 * r3)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task {taskInstr = SMul (Reg rl) (Reg rh) (Reg reg3) (Reg reg4) } stub
  = do
    let  r3 = fromValue $ getRegStub stub reg3
         r4 = fromValue $ getRegStub stub reg4
         (a', b') = (toInt32 (fst r3), toInt32 (snd r3))
         (c', d') = (toInt32 (fst r4), toInt32 (snd r4))
         (x, y) = ( fromIntegral (min (min (a' * c') (a' * d')) (min (b' * c') (b' * d'))) :: Word64,
                    fromIntegral (max (max (a' * c') (a' * d')) (max (b' * c') (b' * d'))) :: Word64 )

         splitWord word (hi, lo)
             = let mask = (2 ^ (hi - lo + 1) - 1) `shiftL` lo
                   in (word .&. mask) `shiftR` lo
         x_lo = fromIntegral (splitWord x (31,0)) :: Word32
         x_hi = fromIntegral (splitWord x (63,32)) :: Word32
         y_lo = fromIntegral (splitWord y (31,0)) :: Word32
         y_hi = fromIntegral (splitWord y (63,32)) :: Word32
         lo = (x_lo, y_lo)
         hi = (x_hi, y_hi)
         stub' = setRegStub stub rl (RegVal lo)
         stub'' = setRegStub stub' rl (RegVal hi)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub'' }

executeInstr_ cycles  task@Task { taskInstr = Sub (Reg reg1) (Reg reg2) (Con con1) } stub
  = do
    let  r2 = getRegStub stub reg2
         reg1' = case r2 of
                      RegVal _ -> r2 - (RegVal (con1, con1))
                      BackVal v2 -> (RegVal v2) - (RegVal (con1, con1))
                      StdVal _ -> r2 - (StdVal con1)
                      MemVal (v2, a) -> RegVal (v2 - (con1, con1))
         stub' = setRegStub stub reg1 reg1'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                              task = Executed task stub' }

executeInstr_ cycles  task@Task { taskInstr = Sub (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = do
    let  r2 = getRegStub stub reg2
         r3 = getRegStub stub reg3
         stub' = setRegStub stub reg1 (r2 - r3)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles  task@Task { taskInstr = Rsb (Reg reg1) (Reg reg2) (Reg reg3) } stub
  = let  r2 = getRegStub stub reg2
         r3 = getRegStub stub reg3
         stub' = setRegStub stub reg1 (r3 - r2)
    in return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                               task = Executed task stub' }

executeInstr_ cycles  task@Task { taskInstr = Rsb (Reg reg1) (Reg reg2) (Con c) } stub
  = do
    let  r2 = fromValue $ getRegStub stub reg2
         stub' = setRegStub stub reg1 (RegVal ((c,c) - r2))
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg) (Con con) } stub
  = do
    let  r = getRegStub stub reg
         stub' = setRegStub stub reg (RegVal (con, con))
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg) (ArithRegShiftL r1 r2) } stub
  = do
    let  r = getRegStub stub reg
         (a,b) = fromValueToInt32 $ getRegStub stub r1
         (x,y) = fromValueToInt32 $ getRegStub stub r2
         low = fromIntegral  (shiftL a (fromInteger x)) :: Word32
         high = fromIntegral (shiftL b (fromInteger y)) :: Word32
         stub' = setRegStub stub reg (RegVal (low, high))
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }


executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg1) (Reg reg2) } stub
  = do
    let  reg2' = case (getRegStub stub reg2, reg2) of
                      (MemVal (val,_), _) -> RegVal val
                      (val, _) -> val
         stub' = setRegStub stub reg1 reg2'
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Movne (Reg reg1) (Con con) } stub
  = do
    let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
        eq = getControlEQ $ control status
        stub' = if eq == 0
                   then  setRegStub stub reg1 (RegVal (con, con))
                   else  stub
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Moveq (Reg reg1) (Con con) } stub
  = do
    let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
        eq = getControlEQ $ control status
        stub' = if eq == 1
                   then  setRegStub stub reg1 (RegVal (con, con))
                   else  stub
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Movls (Reg reg1) (Con con) } stub
  = do
    let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
        lo = getControlLO $ control status
        eq = getControlEQ $ control status
        stub' = if eq == 1 || lo == 1
                   then  setRegStub stub reg1 (RegVal (con, con))
                   else  stub
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Movhi (Reg reg1) (Con con) } stub
  = do
    let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
        hi = getControlHI $ control status
        stub' = if hi == 1
                   then  setRegStub stub reg1 (RegVal (con, con))
                   else  stub
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Movge (Reg reg1) (Reg reg2) } stub
  = do
    let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
        gt = getControlGT $ control status
        eq = getControlEQ $ control status
        stub' = if eq == 1 || gt == 1
                   then  setRegStub stub reg1 $ getRegStub stub reg2
                   else  stub
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg1) (ArithShiftL reg2 off) } stub
  = do
    let  (x,y) = fromValueToInt32 $ getRegStub stub reg2
         shift arg = fromIntegral (shiftL arg (fromInteger off)) :: Word32
         stub' = setRegStub stub reg1 $ RegVal (shift x, shift y)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg1) (ArithShiftR reg2 off) } stub
  = do
    let  (x,y) = fromValueToInt32 $ getRegStub stub reg2
         shift arg = fromIntegral (shiftR arg (fromInteger off)) :: Word32
         stub' = setRegStub stub reg1 $ RegVal (shift x, shift y)
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mov (Reg reg1) (BinShiftR reg2 off) } stub
  = do
    let  (x,y) = fromValueToInt32 $ getRegStub stub reg2
         (x', y') = (intToBinary32 x, intToBinary32 y)
         (x'', y'') = ( binary32ToWord32 (x' `shift` (fromInteger off)),
                        binary32ToWord32 (y' `shift` (fromInteger off)))
         stub' = setRegStub stub reg1 $ RegVal (x'', y'')
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles task@Task { taskInstr = Mvn (Reg reg1) (Con c) } stub
  = do
    let c'' = toInt32 $ complement c
    let c' = complement c
    let stub' = setRegStub stub reg1 (RegVal (c', c'))
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }

executeInstr_ cycles  task@Task { taskInstr = Beq (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral (-offset))
                              else StdVal $ pc' + (fromIntegral offset)
                   lt = getControlLT $ control status
                   eq = getControlEQ $ control status
                   gt = getControlGT $ control status

              stub' <- if   eq == 1
                            then do
                                 let val = equal status
                                     set = status { control = setControlB (control status),
                                                    back = val }
                                     stub'  = setRegStub stub CPSR (CtrVal set )
                                 return $ setRegStub stub' R15 pc''
                            else do
                                 let val1 = lessthan status
                                     val2 = greaterthan status
                                     val' = if lt == 1 && gt == 0
                                                 then val1
                                                 else if lt == 0 && gt == 1
                                                         then val2
                                                         else join val1 val2
                                     clear = status { control = setControlI (clearControlB (control status)),
                                                      back = val' }
                                     stub' = setRegStub stub CPSR (CtrVal clear)
                                 return stub'
              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                        task = Executed task stub' }
        else  do
              let task' = task {  taskInstr = Bne (Rel offset) }
                  status' = status { control = clearControlC cpsr }
                  stub' =  setRegStub stub CPSR $ CtrVal status'
              absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
              let t' = t {  taskInstr = Beq (Rel offset) }
                  s' =  setRegStub s R15 (getRegStub stub R15)
              return $ absTask { task = Executed t' s' }

executeInstr_ cycles  task@Task { taskInstr = Bne (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral $ -offset)
                              else StdVal $ pc' + (fromIntegral offset)
                   lt = getControlLT $ control status
                   eq = getControlEQ $ control status
                   gt = getControlGT $ control status

              stub' <- if   eq == 0 || lt == 1 || gt == 1
                           then do
                                let val1 = lessthan status
                                    val2 = greaterthan status
                                    val' =  if lt == 1 && gt == 0
                                                 then val1
                                                 else if lt == 0 && gt == 1
                                                      then val2
                                                      else join val1 val2
                                    set  = status { control = setControlB (control status),
                                                    back = val' }
                                    stub'  = setRegStub stub CPSR (CtrVal set)
                                return $ setRegStub stub' R15  pc''
                           else do
                                let val = equal status
                                    clear = status { control = setControlI (clearControlB (control status)),
                                                     back = val }
                                    stub'  = setRegStub stub CPSR (CtrVal clear)
                                return stub'

              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                      task = Executed task stub' }
        else  do
              let task' = task {  taskInstr = Beq (Rel offset) }
                  status' = status { control = clearControlC cpsr }
                  stub' =  setRegStub stub CPSR $ CtrVal status'
              absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
              let t' = t {  taskInstr = Bne (Rel offset) }
                  s' =  setRegStub s R15 (getRegStub stub R15)
              return $ absTask { task = Executed t' s' }



executeInstr_ cycles  task@Task { taskInstr = Bgt (Rel offset) }  stub
  = let  CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in if getControlC cpsr == 0
       then  do
             let StdVal pc = getRegStub stub R15
                 pc' = pc - 4
                 pc'' = if  offset < 0
                            then StdVal $ pc' - (fromIntegral $ -offset)
                            else StdVal $ pc' + (fromIntegral offset)

             let lt = getControlLT $ control status
                 eq = getControlEQ $ control status
                 gt = getControlGT $ control status

             stub' <-  if gt == 1
                          then do
                               let val = greaterthan status
                                   set = status { control = setControlB (control status) ,
                                                  back = val }
                                   stub' = setRegStub stub CPSR (CtrVal set)
                               return $ setRegStub stub' R15  pc''
                          else do
                               let val1 = equal status
                                   val2 = lessthan status
                                   val' = join  val1 val2
                                   clear = status { control = setControlI (clearControlB (control status)),
                                                    back = val' }
                                   stub' = setRegStub stub CPSR (CtrVal clear)
                               return stub'
             return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                     task = Executed task stub' }

       else  do
             let task' = task {  taskInstr = Ble (Rel offset) }
                 status' = status { control = clearControlC cpsr }
                 stub' =  setRegStub stub CPSR $ CtrVal status'

             absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
             let t' = t {  taskInstr = Bgt (Rel offset) }
                 s' =  setRegStub s R15 (getRegStub stub R15)
             return $ absTask { task = Executed t' s' }

executeInstr_ cycles  task@Task { taskInstr = Blt (Rel offset) }  stub
  = let  CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in if getControlC cpsr == 0
       then  do
             let StdVal pc = getRegStub stub R15
                 pc' = pc - 4
                 pc'' = if  offset < 0
                            then StdVal $ pc' - (fromIntegral $ -offset)
                            else StdVal $ pc' + (fromIntegral offset)

             let lt = getControlLT $ control status
                 eq = getControlEQ $ control status
                 gt = getControlGT $ control status

             stub' <- if  lt == 1
                          then do
                               let val = lessthan status
                                   set = status { control = setControlB (control status) ,
                                                  back = val }
                                   stub' = setRegStub stub CPSR (CtrVal set)
                               return $ setRegStub stub' R15  pc''
                          else do
                               let val1 = equal status
                                   val2 = greaterthan status
                                   val' = join  val1 val2
                                   clear = status { control = setControlI (clearControlB (control status)),
                                                    back = val' }
                                   stub' = setRegStub stub CPSR (CtrVal clear)
                               return stub'
             return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                     task = Executed task stub' }

       else  do
             let task' = task {  taskInstr = Bge (Rel offset) }
                 status' = status { control = clearControlC cpsr }
                 stub' =  setRegStub stub CPSR $ CtrVal status'
             absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
             let t' = t {  taskInstr = Blt (Rel offset) }
                 s' =  setRegStub s R15 (getRegStub stub R15)
             return $ absTask { task = Executed t' s' }

executeInstr_ cycles  task@Task { taskInstr = Bge (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral $ -offset)
                              else StdVal $ pc' + (fromIntegral offset)

                   lt = getControlLT $ control status
                   eq = getControlEQ $ control status
                   gt = getControlGT $ control status

              stub' <- if  eq == 1 || gt == 1
                           then do
                                let val1 = equal status
                                    val2 = greaterthan status
                                    set = status { control = setControlB (control status),
                                                   back = (join val1 val2) }
                                    stub'' = setRegStub stub CPSR (CtrVal set)
                                return $ setRegStub stub'' R15  pc''
                           else do
                                let val = lessthan status
                                    clear = status { control = setControlI (clearControlB (control status)),
                                                     back = val }
                                    stub' = setRegStub stub CPSR (CtrVal clear)
                                return stub'
              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                      task = Executed task stub' }
        else  do
              let task' = task {  taskInstr = Blt (Rel offset) }
                  status' = status { control = clearControlC cpsr }
                  stub' =  setRegStub stub CPSR $ CtrVal status'
              absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
              let t' = t {  taskInstr = Bge (Rel offset) }
                  s' =  setRegStub s R15 (getRegStub stub R15)
              return $ absTask { task = Executed t' s' }


executeInstr_ cycles  task@Task { taskInstr = Ble (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral $ -offset)
                              else StdVal $ pc' + (fromIntegral offset)

                   lt = getControlLT $ control status
                   eq = getControlEQ $ control status
                   gt = getControlGT $ control status

              stub' <- if  lt == 1 || eq == 1
                           then do
                                let val1 = lessthan status
                                    val2 = equal status
                                    val' = join val1 val2
                                    set  = status { control = setControlB (control status),
                                                    back = val' }
                                    stub' = setRegStub stub CPSR (CtrVal set)
                                return $ setRegStub stub' R15  pc''
                           else do
                                let val = greaterthan status
                                    clear = status { control = setControlI (clearControlB (control status)),
                                                     back = val }
                                    stub' = setRegStub stub CPSR (CtrVal clear)
                                return stub'
              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                      task = Executed task stub' }
        else  do
             let task' = task {  taskInstr = Bgt (Rel offset) }
                 status' = status { control = clearControlC cpsr }
                 stub' =  setRegStub stub CPSR $ CtrVal status'

             absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
             let t' = t {  taskInstr = Ble (Rel offset) }
                 s' =  setRegStub s R15 (getRegStub stub R15)
             return $ absTask { task = Executed t' s' }


executeInstr_ cycles  task@Task { taskInstr = Bls (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral $ -offset)
                              else StdVal $ pc' + (fromIntegral offset)

                   lo = getControlLO $ control status
                   eq = getControlEQ $ control status
                   hi = getControlHI $ control status

              stub' <- if  lo == 1 || eq == 1
                           then do
                                let val1 = lower status
                                    val2 = equal status
                                    val' = join val1 val2
                                    set  = status { control = setControlB (control status),
                                                    back = val' }
                                    stub' = setRegStub stub CPSR (CtrVal set)
                                return $ setRegStub stub' R15  pc''
                           else do
                                let val = higher status
                                    clear = status { control = setControlI (clearControlB (control status)),
                                                     back = val }
                                    stub' = setRegStub stub CPSR (CtrVal clear)
                                return stub'
              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                      task = Executed task stub' }
        else  do
             let task' = task {  taskInstr = Bhi (Rel offset) }
                 status' = status { control = clearControlC cpsr }
                 stub' =  setRegStub stub CPSR $ CtrVal status'
             absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
             let t' = t {  taskInstr = Bls (Rel offset) }
                 s' =  setRegStub s R15 (getRegStub stub R15)
             return $ absTask { task = Executed t' s' }

executeInstr_ cycles  task@Task { taskInstr = Bhi (Rel offset) } stub
  = let CtrVal status@Control { control = cpsr } = getRegStub stub CPSR
    in  if getControlC cpsr == 0
        then  do
              let  StdVal pc = getRegStub stub R15
                   pc' = pc - 4
                   pc'' = if  offset < 0
                              then StdVal $ pc' - (fromIntegral $ -offset)
                              else StdVal $ pc' + (fromIntegral offset)

                   lo = getControlLO $ control status
                   eq = getControlEQ $ control status
                   hi = getControlHI $ control status

              stub' <- if  hi == 1
                           then do
                                let val = higher status
                                    set  = status { control = setControlB (control status),
                                                    back = val }
                                    stub' = setRegStub stub CPSR (CtrVal set)
                                return $ setRegStub stub' R15  pc''
                           else do
                                let val1 = lower status
                                    val2 = equal status
                                    val' = join val1 val2
                                    clear = status { control = setControlI (clearControlB (control status)),
                                                     back = val' }
                                    stub' = setRegStub stub CPSR (CtrVal clear)
                                return stub'
              return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                                      task = Executed task stub' }
        else  do
             let task' = task {  taskInstr = Bls (Rel offset) }
                 status' = status { control = clearControlC cpsr }
                 stub' =  setRegStub stub CPSR $ CtrVal status'
             absTask@AbsTaskState { task = Executed t s } <- executeInstr cycles False task' stub'
             let t' = t {  taskInstr = Bhi (Rel offset) }
                 s' =  setRegStub s R15 (getRegStub stub R15)
             return $ absTask { task = Executed t' s' }

executeInstr_ cycles  task@Task { taskInstr = Bl (Rel offset) } stub
  = let  StdVal pc = getRegStub stub R15
         pc' = pc - 4
         pc'' = if  offset < 0
                    then StdVal $ pc' - (fromIntegral $ -offset)
                    else StdVal $ pc' + (fromIntegral offset)
         stub' = setRegStub stub R14 (StdVal pc)
         stub'' = setRegStub stub' R15  pc''
    in return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                               task = Executed task stub'' }


executeInstr_ cycles  task@Task { taskInstr = B (Rel offset) } stub
  = do
    let  StdVal pc = getRegStub stub R15
         pc' = pc - 4
         pc'' = if  offset < 0
                    then StdVal $ pc' - (fromIntegral $ -offset)
                    else StdVal $ pc' + (fromIntegral offset)
         stub' = setRegStub stub R15  pc''
    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub' }


executeInstr_ cycles  task@Task { taskInstr = Cmp (Reg reg1) op2 } stub
  = do
    let v1 = getRegStub stub reg1
        v2 = case op2 of
                  Con c -> RegVal (c,c)
                  Reg r -> getRegStub stub r

    m0' <- compareRegs2 stub (Reg reg1) op2 0
    m1' <- compareRegs2 stub (Reg reg1) op2 1
    m2' <- compareRegs2 stub (Reg reg1) op2 2
    m3' <- compareRegs2 stub (Reg reg1) op2 3
    m4' <- compareRegs2 stub (Reg reg1) op2 4

    let control2 = bottom { lessthan = m0', equal = m1', greaterthan = m2', lower = m3', higher= m4' }
        pairs = [ (0, m0'), (1, m1'), (2, m2'), (3, m3'), (4, m4')]
        control3' = backwards'' pairs  control2
        stub_ = setRegStub stub CPSR (CtrVal control3')

    return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                            task = Executed task stub_ }


executeInstr_ cycles  task@Task { taskInstr = Str (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Strb (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Strh (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Ldr (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Ldrb (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Ldrh (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Ldrsh (Reg reg1) op2 } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Swi (Con isn) } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Printf (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PThreadCreate (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PThreadExit (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Exit (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexLock (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadCondWait (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexUnlock (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexDestroy (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadCondSignal (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexattrInit (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexattrSetpshared (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = ShmOpen (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = ShmUnlink (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Ftruncate (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Mmap (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Munmap (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexInit (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = PthreadMutexattrDestroy (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Fork (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Waitpid (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Generic  (Con duration) } stub
  = let prop  =  constantBound (fromIntegral duration) cycles
        StdVal sp = getRegStub stub R13
        StdVal fp = getRegStub stub R11
        stub' = setRegStub stub R13 (StdVal (sp+4))
        stub'' = setRegStub stub' R11 (StdVal (fp+4))
    in return $ AbsTaskState { property = prop, stage = MEM, task = Executed task stub }


executeInstr_ cycles  task@Task { taskInstr = Stmfd op1 (Mrg regList) } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = Stmia op1 (Mrg regList) } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }


executeInstr_ cycles  task@Task { taskInstr = Ldmfd op1 (Mrg regList) } stub
  =  return $ AbsTaskState { property = executedALU cycles, stage = MEM,
                             task = Executed task stub }

executeInstr_ cycles  task@Task { taskInstr = i } stub
  = error $ "not implemented " ++ show i


memoryInstr_ cycles task@Task { taskInstr = Str (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
  = do
    let memval = case getRegStub stub reg1 of
                      MemVal val -> RegVal $ fst val
                      RegVal val -> RegVal val
                      StdVal val -> StdVal val
                      Bottom -> Bottom
                      other -> error $ "R " ++ show other

    (stub', addrs) <- readAddrsFrom stub op2
    file' <- removeFromBack file reg1 addrs

    mem' <- writeMemAddrArray addrs memval m

    let file''' = List.foldl (setReg') file' (Map.toList stub')
        task' = task { taskRegisters = file''' , taskShared = mem' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}


memoryInstr_ cycles task@Task { taskInstr = Strb (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
  = do
    let memval = case getRegStub stub reg1 of
                      MemVal ((a, b), addrs) -> RegVal (0x000000FF .&. a, 0x000000FF .&. b)
                      RegVal (a, b) -> RegVal (0x000000FF .&. a, 0x000000FF .&. b)


    (stub', addrs) <- readAddrsFrom stub op2
    mem' <- writeMemAddrArray addrs memval m
    file' <- removeFromBack file reg1 addrs

    let file''' = List.foldl (setReg') file' (Map.toList stub')
        task' = task { taskRegisters = file''' , taskShared = mem' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles task@Task { taskInstr = Strh (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
  = do
    let memval = case getRegStub stub reg1 of
                      MemVal ((a, b), addrs) -> RegVal (0x0000FFFF .&. a, 0x0000FFFF .&. b)
                      RegVal (a, b) -> RegVal (0x0000FFFF .&. a, 0x0000FFFF .&. b)

    (stub', addrs) <- readAddrsFrom stub op2
    mem' <- writeMemAddrArray addrs memval m
    file' <- removeFromBack file reg1 addrs

    let file''' = List.foldl (setReg') file' (Map.toList stub')
        task' = task { taskRegisters = file''' , taskShared = mem' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}



memoryInstr_ cycles task@Task { taskInstr =  Ldr (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
 = do
   (stub', mem', fromMem, memAddrs) <- readValueFrom m stub op2

   stub' <- addToBack file stub reg1 fromMem memAddrs

   let file' = List.foldl (setReg') file (Map.toList stub')
       task' = task { taskRegisters = file' , taskShared = mem' }

   return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}


memoryInstr_ cycles task@Task { taskInstr =  Ldrb (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
 = do
   (stub', mem', fromMem, memAddrs) <- readValueFrom m stub op2

   let fromMem' = case fromMem of
                       Bottom -> RegVal (0,0)
                       val -> let (a, b) = fromValue val
                             in RegVal (0x000000FF .&. a, 0x000000FF .&. b)

   stub' <- addToBack file stub reg1 fromMem' memAddrs

   let file' = List.foldl (setReg') file (Map.toList stub')
       task' = task { taskRegisters = file' , taskShared = mem' }

   return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}


memoryInstr_ cycles task@Task { taskInstr =  Ldrh (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
 = do
   (stub', mem', fromMem, memAddrs) <- readValueFrom m stub op2

   let (a, b)  = fromValue fromMem
       fromMem' = RegVal (0x0000FFFF .&. a, 0x0000FFFF .&. b)

   stub' <- addToBack file stub reg1 fromMem' memAddrs

   let file' = List.foldl (setReg') file (Map.toList stub')
       task' = task { taskRegisters = file' , taskShared = mem' }

   return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles task@Task { taskInstr =  Ldrsh (Reg reg1) op2, taskRegisters = file, taskShared = m } stub
 = do
   (stub', mem', fromMem, memAddrs) <- readValueFrom m stub op2

   let (a, b) = fromValue fromMem
       (a', b') = (0x0000FFFF .&. a, 0x0000FFFF .&. b)
       a'' = if toInt32 a >= 0  then 0x00007FFF .&. a' else a'
       b'' = if toInt32 b >= 0  then 0x00007FFF .&. b' else b'
       fromMem' = RegVal (a'', b'')

   stub' <- addToBack file stub reg1 fromMem' memAddrs

   let file' = List.foldl (setReg') file (Map.toList stub')
       task' = task { taskRegisters = file' , taskShared = mem' }

   return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles task@Task { taskInstr = Stmfd op1 (Mrg regList), taskRegisters = file, taskShared = m } stub
  = do
    let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
        StdVal addr = getRegStub stub reg

    let storeRegs addr [] stack
           = return (addr, stack)
        storeRegs addr (r : rs) stack
           = do let addr' = addr - 4
                    val  = getRegStub stub r
                let st' = push stack (addr', val)
                storeRegs addr' rs st'

    (addr', st')  <- storeRegs addr (List.reverse regList) (stack m)

    let stack' = stackToMap st'

    stub' <- if writeBack
               then return (setRegStub stub reg (StdVal addr'))
               else return stub

    let transferStack mem ([])
            = return mem
        transferStack mem (s:ss)
            = do let (addr, val) = s
                 mem' <- writeMemWord mem addr val
                 transferStack mem' ss

    m' <- transferStack m (Map.toList stack')

    let file'' = List.foldl (setReg') file (Map.toList stub')
        m'' = m' { stack = (listToStack . Map.toList) stack' }
        task' = task { taskRegisters = file'', taskShared = m'' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles task@Task { taskInstr = Stmia op1 (Mrg regList), taskRegisters = file, taskShared = m } stub
  = do
    let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
        addr = case getRegStub stub reg of
                    StdVal sp -> sp
                    MemVal ((sp1,sp2),_) -> if sp1 == sp2 then sp1 else error "multiple at inside stmia"

    let storeRegs addr [] stack
           = return (addr, stack)
        storeRegs addr (r : rs) stack
           = do let val  = getRegStub stub r
                    addr' = addr + 4
                let st' = push stack (addr', val)
                storeRegs addr' rs st'

    (addr', st')  <- storeRegs addr (List.reverse regList) (stack m)

    let stack' = stackToMap st'

    stub' <- if writeBack
               then return (setRegStub stub reg (StdVal addr'))
               else return stub

    let transferStack mem ([])
            = return mem
        transferStack mem (s:ss)
            = do let (addr, val) = s
                 mem' <- writeMemWord mem addr val
                 transferStack mem' ss

    m' <- transferStack m (Map.toList stack')

    let file'' = List.foldl (setReg') file (Map.toList stub')
        m'' = m' { stack = (listToStack . Map.toList) stack' }
        task' = task { taskRegisters = file'', taskShared = m'' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles task@Task { taskInstr = Ldmfd op1 (Mrg regList), taskRegisters = file, taskShared = m } stub
  = do
    let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
        StdVal addr = getRegStub stub reg
        st = stack m

    let inside  = List.map (\pair -> fst pair) (stackToList (stack m))

    let loadRegs s addr [] stack
            = return (addr, stack, s)
        loadRegs s addr (r : rs) stack
            = do let (top_addr,_) = top stack
                 ((addr',val), rest) <-
                   if top_addr == addr
                      then return (pop stack)
                      else if elem addr inside
                              then do
                                   let f = (\(a,v) -> if a < addr then True else False)
                                       (left, stack') = partitionStack f stack
                                       (top, stack'') = pop stack'
                                   return (top, appendStack left stack'')
                              else do
                                   (v, _) <- readMemWord m addr
                                   return ((addr,v), stack)

                 let s' = setRegStub s r val
                 loadRegs s' (addr + 4) rs rest


    (addr', st', stub')  <- loadRegs stub (addr) regList (stack m)

    let stack' = stackToMap st'

    let transferStack mem ([])
            = return mem
        transferStack mem (s:ss)
            = do let (addr, val) = s
                     val' =  if elem addr inside then val else bottom
                 mem' <-  writeMemWord mem addr val'
                 transferStack mem' ss

    let file' = if writeBack
                   then error ("ldmfd index update")
                   else List.foldl (setReg') file (Map.toList stub')

    m' <- transferStack m (Map.toList stack')

    let m'' = m' { stack = (listToStack . Map.toList) stack' }
        pc' = case getReg file' R15 of
                   StdVal pc -> pc
                   Bottom -> 0
        task' = task { taskRegisters = file' , taskShared = m'', taskNextPc = pc' }

    return $ AbsTaskState { property = memoryExchange cycles, stage = WB, task = Done task'}

memoryInstr_ cycles t@Task { taskRegisters = file, taskMemory = m } stub
 = do
   let file' = List.foldl (setReg') file (Map.toList stub)
       StdVal target = getReg file' R15
       task' = t { taskRegisters = file', taskNextPc = target}
   return $ AbsTaskState { property = writeBack cycles, stage = WB, task = Done task' }


readAddrsFrom
  :: Stubs
  -> Operand
  -> IO (Stubs, [Word32])

readAddrsFrom stub op2
  = case op2 of
         Con addr -> do
             return (stub, addr:[])
         Ind reg2 -> do
             let (l, u) = fromValue $ getRegStub stub reg2
                 addrs = resolveAddresses [max 0 l, min stackSize u] 0
             return (stub, addrs)
         Bas reg2 offset -> do
             let addrs
                   = case getRegStub stub reg2 of
                          PtrVal (l,u) -> resolveAddresses [max 0 l, min stackSize u] offset
                          SpVal (l,u) -> resolveAddresses [max 0 l, min stackSize u] offset
                          StdVal addr -> (addr + (fromInteger offset)):[]
                          Bottom -> []
                          RegVal (l,u) -> resolveAddresses [max 0 l, min stackSize u] offset
                          other -> error $ "read addresses " ++ show other
             return (stub, addrs)
         BasShift baseReg (ArithShiftL shiftReg bits) -> do
             let (a, b) = fromValueToInt32 $ getRegStub stub shiftReg
                 addrs = fromValue $ getRegStub stub baseReg
                 low =  (shiftL a (fromInteger bits))
                 high = (shiftL b (fromInteger bits))
                 offsets = [low, low + 4 .. high]
                 collect offset = resolveAddresses [max 0 (fst addrs), min stackSize (snd addrs)] offset
                 addrs' = List.nub $ List.concat $ List.map collect offsets
             return (stub, addrs')
         BasShift baseReg (Reg shiftReg) -> do
             let (low, high) = fromValueToInt32 $ getRegStub stub shiftReg
                 addrs = fromValue $ getRegStub stub baseReg
             let offsets = [low, low + 4 .. high]
                 collect offset = resolveAddresses [max 0 (fst addrs), min stackSize (snd addrs)] offset
                 addrs' = List.nub $ List.concat $ List.map collect offsets
             return (stub, addrs')


readValueFrom
  :: SharedMemory
  ->  Stubs
  ->  Operand
  ->  IO (Stubs, SharedMemory, Value, [Word32])

readValueFrom m stub op2
  = do
    (stub', addrs) <- readAddrsFrom stub  op2
    (val, m') <- readMemAddrArray addrs [] Bottom m
    return (stub', m', val, addrs)



removeFromBack
  :: Registers
  -> RegisterName
  -> [Word32]
  -> IO Registers

removeFromBack file reg1 addrs
   = case getReg file CPSR of
          CtrVal status ->  do
             let remove (MemVal (v, b))
                   = let b' = List.foldl (\accum a -> List.delete a accum) b addrs
                     in MemVal (v,b')
                 remove r = r
                 backs' = Map.adjust (\b -> remove b) reg1 (back status)

             return $  setReg file CPSR (CtrVal (status {back = backs'} ))
          _ -> return file

addToBack
  :: Registers
  ->  Stubs
  ->  RegisterName
  ->  Value
  ->  [Word32]
  ->  IO Stubs

addToBack file stub reg1 fromMem memAddrs
   = do
     let backs = case getReg file CPSR of { CtrVal status -> back status;  _ -> Map.empty }
         postBack
            =  if fromMem == Bottom
                  then Bottom
                  else let read = fromValue fromMem
                       in if  Map.member reg1 backs
                              then case getRegStub stub reg1 of
                                        Bottom -> MemVal (read, memAddrs)
                                        StdVal _ -> MemVal (read, memAddrs)
                                        RegVal _ -> MemVal (read, memAddrs)
                                        MemVal (preVal, preAddrs) ->
                                           if preAddrs == memAddrs
                                              then MemVal (preVal, preAddrs)
                                              else MemVal (read, memAddrs)
                              else MemVal (read, memAddrs)

     return $ setRegStub stub reg1 postBack

writeMemAddrArray
  :: [Address]
  -> Value
  -> SharedMemory
  -> IO SharedMemory

writeMemAddrArray [] _ mem
   = return mem
writeMemAddrArray (addr:as) val mem
   = do
     mem' <- writeMemWord mem addr val
     writeMemAddrArray as val mem'


readMemAddrArray
  :: [Address]
  -> [Address]
  -> Value
  -> SharedMemory
  -> IO (Value, SharedMemory)

readMemAddrArray [] _ i m
  = return (i, m)

readMemAddrArray (addr:as) backs i m
  = do
    (w, m') <- readMemWord m addr
    let update a b = case cmp a b of
                          Nothing -> a
                          _ -> join a b
        fromMem = update i w
    readMemAddrArray as backs fromMem m'

-- | Obtain the lookup addresses using an offset
resolveAddresses
  :: [Word32]
  ->  Integer
  ->  [Word32]

resolveAddresses addrs offset
  = let f addr =  let  addr' = addr + (fromInteger offset)
                  in if toInt32 addr' < 0
                        then 0
                        else addr'
    in  case List.map f addrs of
             [] -> []
             [a] -> [a]
             [l,u] -> if l == u
                        then [ u ]
                        else let start = 4 * (l `div` 4)
                                 stop = 4 * (u `div` 4)
                             in [start, start + 4 .. stop]

data Condition = LessThan | Equal | GreaterThan | LowerThan | HigherThan

backwards' :: Control ->  Control

backwards' c@Control{ lessthan, equal, greaterthan, lower, higher }
  = if lessthan /= bottom
       then let cpsr = setControlLT (control c)  in c { control = cpsr }
    else if equal /= bottom
       then let cpsr = setControlEQ (control c)  in c { control = cpsr }
    else if greaterthan /= bottom
       then let cpsr = setControlGT (control c)  in c { control = cpsr }
    else if lower /= bottom
       then let cpsr = setControlLO (control c)  in c { control = cpsr }
    else if higher /= bottom
       then let cpsr = setControlHI (control c)  in c { control = cpsr }
    else c

backwards'' :: [(Int, Stubs)] -> Control -> Control
backwards'' pairs c
  = let  f g cpsr = g cpsr
         k s = List.all (==bottom) (Map.elems s)
         update c@Control{ control = ctr } (0, lt) = if k lt then c else c { control = f setControlLT ctr }
         update c@Control{ control = ctr } (1, eq) = if k eq then c else c { control = f setControlEQ ctr }
         update c@Control{ control = ctr } (2, gt) = if k gt then c else c { control = f setControlGT ctr }
         update c@Control{ control = ctr } (3, lo) = if k lo then c else c { control = f setControlLO ctr }
         update c@Control{ control = ctr } (4, hi) = if k hi then c else c { control = f setControlHI ctr }
    in List.foldl update c pairs


backwards
  :: [(Condition, Bool)]
  ->  Control
  ->  Control

backwards pairs ctr
  = let back c (LessThan, False) = c { lessthan = bottom }
        back c (LessThan, True) = let cpsr = setControlLT (control c)  in c { control = cpsr }

        back c (Equal, False) = c { equal = bottom }
        back c (Equal, True) = let cpsr = setControlEQ (control c)  in c { control = cpsr }

        back c (GreaterThan, False) = c { greaterthan = bottom }
        back c (GreaterThan, True) = let cpsr = setControlGT (control c)  in c { control = cpsr }

        back c (LowerThan, False) = c { lower = bottom }
        back c (LowerThan, True) = let cpsr = setControlLO (control c)  in c { control = cpsr }

        back c (HigherThan, False) = c { higher = bottom }
        back c (HigherThan, True) = let cpsr = setControlHI (control c)  in c { control = cpsr }

    in List.foldl back ctr pairs

