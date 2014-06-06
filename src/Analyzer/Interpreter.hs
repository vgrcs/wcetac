-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Interpreter
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Analyzer.Interpreter ( transf, targetPc,

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Prelude hiding (read)
import System.IO.Unsafe
import Data.Map hiding (update)
import Data.Word
import Data.Maybe
import qualified Data.List as List
import Control.Monad
import Control.Exception
import Control.Monad.State hiding (lift,join)

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.Chaotic
import Analyzer.ProgramFlow
import Analyzer.Chaotic
import Analyzer.Certificate
import Analyzer.PipelineModel
import Analyzer.ScreenPrinter
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.Container
import Analyzer.ValueAbstraction
import Analyzer.Interleavings
import Arm.CPU
import Arm.Pipeline hiding (update)
import Arm.Instruction
import Arm.Operand
import Arm.RegisterName
import Arm.Register



instance (Show a, Eq a, Ord a, Cost a, Show (Core a)) => Abstractable (CPU a) where
  apply s f
    = do invs' <- (f . invs) s
         instrument s invs'

  lift r f side cert
    =  do
       cpu <- read cert r
       let cpu' = unsafePerformIO $ updateCallingContext r cpu
       cpu'' <- f cpu'
       let r' = unsafePerformIO $ returnCallingContext r cpu''
       cert'' <- chaotic r' cert side cpu''
       return (r', cert'')

-- | Instantiation of the transition functions
transf
   :: (Show a, Cost a, Eq a, Ord a, FiveStagePipeline a, Abstractable (CPU a))
   => ACC
   -> (Rel (St (CPU a)))
   -> (RelAbs (St (CPU a)))

transf side r
   =  \s -> do
        let sim (Exec i) = \cpu -> return $ unsafePerformIO $
                                   evalStateT simulate
                                   SimContext { simRel = r,
                                                simCPU = cpu {active = running s},
                                                simTask = Nothing,
                                                simInstrs = i:[] }
            sim (Threaded i) = \cpu -> return $ unsafePerformIO $
                                       evalStateT simulate
                                       SimContext { simRel = r,
                                                    simCPU = cpu {active = running s},
                                                    simTask = Nothing,
                                                    simInstrs = i:[]}
            sim (Interleaved i) = \cpu -> return $ unsafePerformIO $
                                          evalStateT simulate
                                          SimContext { simRel = r,
                                                       simCPU = cpu {active = running s},
                                                       simTask = Nothing,
                                                       simInstrs = i:[]}
            sim (Cons i l) = let instrs = toListExpr (Cons i l)
                             in \cpu -> return $ unsafePerformIO $
                                        evalStateT simulate
                                        SimContext { simRel = r,
                                                     simCPU = cpu {active = running s},
                                                     simTask = Nothing,
                                                     simInstrs =  instrs }
            step = (sim . expr) r
            eval = lift r step side
        apply s eval


targetInstr
  :: (Stateable a) => Instruction
  ->  Rel a
  ->  [Instruction]

targetInstr i r
  = let e = expr r
        l = toListExpr e
    in [i]


targetSink
  :: (Stateable a) =>  Instruction
  ->  Rel a
  ->  Rel a

targetSink i r
  = let e = expr r
        l = toListExpr e
        p = case List.elemIndex i l of
                 Just x -> x
                 Nothing -> error $ "targetSink"
        id = 1 + (toInteger p) + (ppoint . source) r
        ret = NodeL $ (identifier (source r)) { labelId = id }
        r' = adjustOut r ret
    in  case (sink r, source r, p == 1 - length l)  of
             (ExitL a, HookL b, True) -> r
             _ -> r'

simulate
   :: (Show a, Eq a, FiveStagePipeline a, Ord a) =>
   StateT (SimContext a) IO (CPU a)

simulate = merge >>= collection squash >>= update


collection ::(Instruction -> (PState a) -> StateT (SimContext a) IO (Task, (PState a)))
              -> (PState a) -> StateT (SimContext a) IO (Task, [(PState a)])

collection f s
  = do
    (i: is) <- gets simInstrs
    (s', y) <- next i s >>= f i
    if is == []
       then return (s', y:[])
       else do
            (s'', ys) <- withStateT (\c -> c { simTask = Just s', simInstrs = is} ) (collection f y)
            return (s'', y : ys)

next :: Instruction -> PState a -> StateT (SimContext a) IO (PState a)
next instruction input
  =  do
     rel <- gets simRel
     cpu <- gets simCPU
     pc <- liftIO $ targetPc_ rel instruction (context cpu)
     let t' = List.filter (\a -> fst a /= instruction ) (targets input)
         targets' = List.nub $ (instruction, pc ):t'
     return input { targets = targets'}


findTarget i targetPc state@PState { targets = t }
  =  let t' = List.filter (\a -> fst a /= i ) t
     in List.nub $ t' ++ (i, targetPc ):[]


targetPc_ r (Ldmfd _ _) []
  = return 0

targetPc_ r (Ldmfd _ _) (c:[])
  = return $ fromIntegral $ 4 * (ppoint c)

targetPc_ r (Ldmfd _ _) (c:cs)
  =  do
     let pc =  let hook = takeWhile (== c) cs
               in case hook of
                       [] -> fromIntegral $ 4 * (ppoint c)
                       (h:hs) -> fromIntegral $ 4 * (ppoint h)
     return pc

targetPc_ r i _
  =  do
     let l = ( toListExpr . expr) r
         p = fromJust $  List.elemIndex i l
         id = if branchInstr i
                 then (ppoint . sink) r
                 else 1 + (toInteger p) + (ppoint . source) r
         pc = fromIntegral $ 4 * id
     return pc

targetPc (ExitL l, _) []
  = return 0

targetPc (ExitL l,_) (c:[])
  = return $ fromIntegral $ 4 * (ppoint c)

targetPc (ExitL l,_) (c:cs)
  =  do
     let pc =  let hook = takeWhile (== c) cs
               in case hook of
                       [] -> fromIntegral $ 4 * (ppoint c)
                       (h:hs) -> fromIntegral $ 4 * (ppoint h)
     return pc

targetPc (label,_) _
  =  do
     let pc = fromIntegral $ 4 * (ppoint label)
     return pc

squash
 :: (FiveStagePipeline a, Show a, Ord a) => Instruction
 -> PState a
 -> StateT (SimContext a) IO (Task, PState a)

squash instr pipe
  = do
    rel <- gets simRel
    t <- gets simTask

    let pipe' = case t of
                     Nothing -> pipe
                     Just t1 -> pipe { regfile = taskRegisters t1, mem = taskMemory t1 }
        go state =
           do
           state' <- liftIO $ step (sink rel) instr state
           done <- liftIO $ isDone instr state'
           case  done of
                 Nothing -> go state'
                 Just task ->
                    if  instr == taskInstr task
                        then liftIO $ return (task, keep state')
                        else error ("instruction mismatch " ++ show (taskInstr task))
    go pipe'
