-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.RelationalAbs
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
{-# LANGUAGE FlexibleContexts #-}
module Analyzer.RelationalAbs ( abstractProgram, TraceState (..), ProcedureControl (..),
                                filterSameSourceOrSink

) where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Monad.State hiding (put)
import Data.Word
import Data.List

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Instruction
import Analyzer.Semantics
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.MOP hiding (insideLoop)
import Arm.RegisterName
import qualified Arm.Operand as Op
import Arm.Operand hiding (Rel)



data ProcElem = BlCall Proc
               -- | BlRoot Proc
               deriving (Show, Eq)

fromElem (BlCall p) = p

data ProcedureControl  =  Call    [ProcElem]
                       |  Return  [ProcElem]
                       deriving (Show)

type SymbolTable = [(String, Word32)]

data TraceState = TraceState  { proc :: Proc,
                                control :: ProcedureControl,
                                table :: SymbolTable,
                                call :: Bool  }

defaultPostBl = BlCall mainProc
mainProc = Proc { procId = 1 , section = "main", procName = "main" }

instance Initialize TraceState where
  initial = TraceState {proc = initial, control = Call ([defaultPostBl]) , table = [], call = False}


abstractProgram
  :: [Instruction]
  -> Integer
  -> TransSys (St a)
  -> StateT TraceState IO (TransSys (St a))

abstractProgram [] end  (TransSys l)
  =  do
     let l'' =  nubBy (\ r1 r2 -> ((procName . parent . sink) r1 == (procName . parent . sink) r2)
                                 && (procName . parent . sink) r1 == "main" && exit (sink r1) && exit (sink r2) ) l
         l' = TransSys $ (drop 1 . sort) l''
     return l'

abstractProgram (i:is) end (TransSys l)
  =  do
     let r = maximumBy (\r s -> compare r s) l
         (after, at) = ppoints r

     labels <- processInstrunction end l r (i:is)

     let  make l (after, at)
           = let  triple = Rel (setLabel initial after, Exec i, setLabel initial at )
             in (l ++ triple:[])

     let l' = foldl make l labels

     abstractProgram is end  (TransSys l')


processStep
  :: (Stateable a) => [Rel a]
  -> Rel a
  -> Instruction
  -> StateT TraceState IO [(Label, Label)]

processStep l r i
  = do
    p@Proc { procId = id, procName = name } <- gets proc
    lTab <- gets table
    afterCall <- gets call

    let (after, at) = ppoints r
        at' = ppoint at
        l' = filterSameSourceOrSink l
        pc = fromIntegral ((length l') - 1) * 4 :: Word32

    (_,sec) <- destination lTab pc
    p' <- case i of
               Mov (Reg IP) (Reg SP) -> return Proc { procId =  id, section = sec, procName = sec }
               _ -> return Proc { procId =  id, section = sec, procName = name }

    let source = Identifier { labelId = at' + 1, procedure = p' , ilvpos = Nothing}
        sink = Identifier { labelId = at' + 2, procedure = p' , ilvpos = Nothing}

    case i of
         Mov (Reg IP) (Reg SP) -> do
             modify (\ state@TraceState {} -> state {proc = succ p', call = False} )
             return [(NodeL sink, RootL source)]
         _ -> if afterCall
                then do
                     modify (\ state@TraceState {} -> state {call = False} )
                     return [(NodeL sink, HookL source)]
                else return [(NodeL sink, NodeL source)]



processInstrunction
  :: (Stateable a) =>  Integer
  ->  [Rel a]
  ->  Rel a
  -> [Instruction]
  -> StateT TraceState IO [(Label, Label)]

processInstrunction _ l r (Bl (Op.Rel offset):is)
  = do
    p@Proc { procId = id, procName = name } <- gets proc
    lTab <- gets table

    let (after, at) = ppoints r
        l' = filterSameSourceOrSink l
        drop = 1 + length l - length l'
        pc = fromIntegral (((length l') - 1) * 4 + fromInteger offset) :: Word32

    let dest = if offset /= 0
                  then case findIndex (\(_,pc'') -> pc == pc'') lTab of
                            Just d ->  let (s, _) = lTab !! d
                                       in Just $ Proc { procId = d, section = s, procName = name }
                            Nothing -> error ("missing procedure " ++ show pc ++ ",offset " ++ show offset
                                       ++  "\n" ++ show lTab ++ "\n" ++ show (TransSys l))
                  else Nothing
    (state', rel) <- processBl dest at after offset drop
    if offset /= 0
       then modify (\ state@TraceState {} -> state {control = state', call = True} )
       else return ()
    return [rel]


processInstrunction end _ r (Ldmfd a b : is)
  = do
    let (after, at) = ppoints r
    (_, state', rel) <- processLdmfd at after end
    modify (\ state@TraceState {} -> state {control = state'} )
    return rel

processInstrunction end _ r (PThreadExit _ : is)
  =  do
     let (after, at) = ppoints r
     (_, _, rel) <- processLdmfd at after end
     return rel


processInstrunction _ l r (B (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processB l offset at after

processInstrunction _ l r (Bne (Op.Rel offset) : is )
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Beq (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Bgt (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Blt (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Bge (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Ble (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (Bls (Op.Rel offset) : is)
  =  do
     let (after, at) = ppoints r
     processCondBranch l is offset at after

processInstrunction _ l r (i : is)
  = processStep l r i


processBl
  :: Maybe Proc
  -> Label
  -> Label
  -> Integer
  -> Int
  -> StateT TraceState IO (ProcedureControl, (Label, Label))

processBl Nothing at after offset drop
  = let at' = ppoint at
        source = succ at
        sink = case at of
                    NodeL l -> NodeL l { labelId = succ (succ at') }
                    HookL l -> NodeL l { labelId = succ (succ at') }
                    _ -> error (show (at, after))
    in return (Call [], (sink, source))


processBl (Just p') at after offset drop
  = do
    control <- gets control
    afterCall <- gets call

    let (label, hook) = case after of
                             NodeL l -> (l, procedure l)
                             CallL l -> (l, procedure l)

        at' = ppoint at
        after' = (ppoint at) + (offset `div` 4)
        source = if afterCall
                    then HookL label { labelId =  at'+ 1, procedure = hook}
                    else NodeL label { labelId =  at'+ 1}
        rel = (CallL label { labelId =  after'+1, procedure = p'}, source)

    case control of
         Call bls
          -> do
             let new = bls ++ (BlCall p' ):[]
             return (Call new, rel)

         Return bls
          -> do
             let new = bls ++ (BlCall p'):[]
             return (Call new, rel)

processLdmfd
  :: Label
  -> Label
  -> Integer
  -> StateT TraceState IO (Proc, ProcedureControl, [(Label, Label)])

processLdmfd at (NodeL label@Identifier { procedure = p } ) end
  = do
    ctr <- gets control

    let at' = ppoint at
        exit = ExitL Identifier { labelId = -1, procedure = p, ilvpos = Nothing }
    case ctr of
         Call (bl:bls) -> do
            let rel = if (fromElem . last) (bl:bls) ==  p && length (bl:bls) > 1
                         then (exit, NodeL label { labelId = at'+ 1 }):
                              (exit, NodeL label { labelId = at'+ 1 }):[]
                         else (exit, NodeL label { labelId = at'+ 1 }):[]

                pop (BlCall a) (BlCall b) = procName a == section b
                state' =  Return $ deleteBy pop (BlCall p) (bl:bls)
            return  (succ p, state', rel)

         Return (bl:bls)
          -> do
             let state' = Return bls
                 rel = (exit, NodeL label { labelId =  at' + 1 })
             return (p, state', [rel])

         Return [] -> return (p, Return [], [])
         Call [] -> let rel = (exit, NodeL label { labelId = at'+ 1 }):[]
                    in return  (succ p, Call [], rel)


processB
  :: (Stateable a) =>
  [Rel a] -> Integer -> Label -> Label
  -> StateT TraceState IO [(Label, Label)]

processB l offset at (NodeL after)
  = do
    afterCall <- gets call
    p@Proc { procId = id, procName = name } <- gets proc
    modify (\ state@TraceState {} -> state {call = False} )
    if offset > 0
       then let after' = (ppoint at) + (offset `div` 4)
                sink = NodeL after { labelId = after'+1 }
                source = if afterCall
                            then HookL after {labelId = (ppoint at)+1,  procedure = p }
                            else NodeL after {labelId = (ppoint at)+1  }
            in return [(sink, source)]
       else branchWithNegOffset l offset at (NodeL after)

processB l offset at (CallL after)
  = do
    afterCall <- gets call
    p@Proc { procId = id, procName = name } <- gets proc
    modify (\ state@TraceState {} -> state {call = False} )
    if offset > 0
       then let after' = (ppoint at) + (offset `div` 4)
                sink = if afterCall
                          then CallL after { labelId = after'+1, procedure = p }
                          else CallL after { labelId = after'+1 }
                source = if afterCall
                            then  HookL after {labelId = (ppoint at)+1, procedure = p }
                            else  NodeL after {labelId = (ppoint at)+1  }
            in return [(sink, source)]
       else branchWithNegOffset l offset at (NodeL after)

processB l offset at after
  = error $ "processB " ++ show after



branchWithNegOffset
  :: (Stateable a) =>
     [Rel a] -> Integer -> Label -> Label
     -> StateT TraceState IO [(Label, Label)]

branchWithNegOffset l offset at ( NodeL lafter@Identifier { labelId =  af } )
  =  let after' =  af + offset `div` 4
         skip = blockSize l after' ( NodeL lafter )
         after'' = after' + skip
         sink = NodeL lafter { labelId = after'' }
         source = Head lafter { labelId = (ppoint at)+1 }
     in return [(sink,  source)]

branchWithNegOffset l offset at ( CallL lafter@Identifier { labelId =  af } )
  =  let after' =  af + offset `div` 4
         skip = blockSize l after' ( CallL lafter )
         after'' = after' + skip
         sink = CallL lafter { labelId = after'' }
         source = Head lafter { labelId = (ppoint at)+1 }
     in return [(sink, source)]



branchWithPosOffset
  :: Labeled a =>
  [Instruction] -> Integer -> a -> Label
  -> StateT TraceState IO [(Label, Label)]

branchWithPosOffset is offset at (NodeL label@Identifier { labelId = af })
  = let after' = af + (offset `div` 4)
        sink  = NodeL  label { labelId = after' }
        source = Head label { labelId = (ppoint at)+1 }
    in return ((sink, source):[])


processCondBranch
  :: (Stateable a) => [Rel a]
  -> [Instruction]
  -> Integer
  -> Label
  -> Label
  -> StateT TraceState IO [(Label, Label)]

processCondBranch l is offset at after
  = do
    lTab <- gets table
    let l' = filterSameSourceOrSink l
        drop = fromIntegral (1 + length l - length l') :: Word32
        pc = fromIntegral ((length l') - 1) * 4 :: Word32
        NodeL label@Identifier { procedure = p } = after

    jump <- if offset > 0
               then branchWithPosOffset is offset at after
               else branchWithNegOffset l' offset at after

    let skip = (NodeL label { labelId = (ppoint at)+2 } ,
                NodeL label { labelId = (ppoint at)+1 } )

    return (jump ++ skip:[])

destination
  :: SymbolTable
  ->  Word32
  ->  StateT TraceState IO (Int, String)

destination lTab pc
  =  case findIndices (\(_,pc'') -> pc'' <= pc) lTab of
          [] -> error ("destination " ++ show pc ++ "  " ++ show lTab)
          is -> do
                   let (s, _) = lTab !! (last is)
                   return (last is, s)


filterSameSourceOrSink
  :: Stateable a => [Rel a]
  -> [Rel a]

filterSameSourceOrSink l
  = let f accum (Rel (s2,i,s1))
          = case accum of
                 [] -> (Rel (s2,i,s1)):[]
                 xs -> let  (Rel (s2',i', s1')) = last xs
                            sameSource = ppoint (getLabel s1) == ppoint (getLabel s1')
                            sameSink   = ppoint (getLabel s2) == ppoint (getLabel s2') &&  i==i'
                       in  if sameSource || sameSink
                              then  accum
                              else  accum ++ (Rel (s2,i, s1)):[]
    in foldl f [] l



blockSize
  :: Stateable a => [Rel a]
   -> Integer
   -> Label
   -> Integer

blockSize l after' after
  = let f r = let at' = source r
              in if (ppoint at') >= after' && (ppoint at') <= (ppoint after)
                    then True
                    else False
        l'=  map source [ r | r <- l, f r ]
        a = length l'
        b = length $ nub l'
     in toInteger (a - b)

