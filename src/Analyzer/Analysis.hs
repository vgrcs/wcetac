-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Analysis
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Analyzer.Analysis ( analysis, ReductionMode (..), transformACC, defaultCFG,
                           Relations

) where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Map as Map
import Data.List
import Data.Word
import Control.Monad
import Control.DeepSeq
import Control.Monad.State hiding (put)
import Control.Monad.Reader hiding (lift,join)
import Control.Monad.Writer hiding (lift,join)

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Loader
import Arm.Program
import Arm.Instruction
import Arm.Assembler
import Arm.Pipeline hiding (infeasible)
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.Certificate
import Analyzer.Semantics hiding (infeasible)
import Analyzer.RelationalAbs
import Analyzer.Compiler
import Analyzer.Threads
import Analyzer.IR
import Analyzer.PipelineModel
import Analyzer.MOP hiding (insideLoop)
import Analyzer.GraphMLBackend


data ReductionMode = NotReduced | Reduced deriving (Show, Eq)



initialize
  :: (Show a,  Cost a, Ord a) => (CPU a)
  -> Integer
  -> Integer
  -> Invs (CPU a)

initialize cpu point size
  =  let size' = if point == 0 then size + 1 else size
         empty = replicate (1+fromInteger size')
                     (Node { value = bottom, stableFixpoint = False,
                             stableValue = [], insideLoop = No,
                             contexts = [], redirect = Nothing, stableLoop = False,
                             infeasibleNode = False })

         points = [-1] ++ [point ..size]
         cert = Map.fromList $ zip points empty
         start = Node { value = cpu, stableFixpoint = False,
                        stableValue = [], insideLoop = No,
                        contexts = [], redirect = Nothing, stableLoop = False,
                        infeasibleNode = False }
     in Map.insert point start cert

resetCert :: CPU a -> Integer -> Invs (CPU a) -> Invs (CPU a)
resetCert cpu point acc
  = let acc_ = Map.map (\node -> node { contexts = [] } ) acc
        start = acc Map.! point
        start_ = start { value = cpu, contexts = [] }
    in Map.insert point start_ acc_

analysis
  :: (Show a, Cost a, Ord a, FiveStagePipeline a, Show (Node (CPU a)), Show (Core a)) => String
  -> Maybe (Cert (CPU a))
  -> ReductionMode
  -> IO (Relations a, Relations a, Relations a, Invs (CPU a), EdgeCount, EdgeCount)

analysis str cert mode
  = do progOrError <- asmString str
       let (lTab, program)  = case progOrError of
                                   Left parsed -> parsed
                                   Right err -> error ("Parser: " ++ show err)
       cpu <- loadProgram program bottom

       let instrs_ = instructions program
           instrs = filter (\i -> case i of
                                       Ann1 _ -> False
                                       Ann2 _ _ -> False
                                       Ann3 _ _ _ -> False
                                       _ -> True ) instrs_
           s = initial
           s' =  case cert of
                      Nothing ->  initial
                      Just c@Cert { invariants = i, edgeCount = e } ->
                        initial { invs = i, edges = e }

       let emptyRelation = Rel (s', Exec Nop, s)
           lTab' = filter (\(s, (w, t)) -> case t of {Code -> True; _ -> False} ) lTab
           lTab'' = map (\(s,(w,t)) -> (s,w)) lTab'
           trace = initial { table = lTab'' }

       (TransSys rel, st )
         <- runStateT  (abstractProgram instrs ((toInteger.length) instrs) (TransSys [emptyRelation]))
                       trace
       accAnalysis cpu cert rel program instrs mode



defaultCFG
  :: (Cost a, Show a, Ord a) =>
     Maybe (Cert (CPU a))
     -> CPU a
     -> Relations a
     -> IO (Label, Invs (CPU a), EdgeCount, CFG (St (CPU a)), St (CPU a), Relations a, Integer)

defaultCFG fix cpu rel
  = do
    let startLabel = mopStartLabel rel
        r = (maximum rel)
        size = (ppoint . source) r

        (invsS, iterationsS, edgesS)
            = case  fix of
              Nothing -> (initialize cpu (ppoint startLabel) size, bottom, bottom)
              Just c -> (invariants c, iterations c, edgeCount c)

        initialSt = St { labelSt = initial, invs = invsS, edges = edgesS, activeSt = (0,0) }

    cfg <- runStateT (mop EmptyGraph)
           initial { relational = rel, startLabel = startLabel, noConc = True, debug = False }
    let cfg' = (normalize . fst) cfg
    let rel'' =  toList cfg'
    return (startLabel, invsS, iterationsS, cfg', initialSt, rel'', size)


type Relations a = [Rel (St (CPU a))]

accAnalysis
  :: (Show a, Cost a,  Ord a, FiveStagePipeline a, Show (Node (CPU a)), Show (Core a)) => CPU a
  -> Maybe (Cert (CPU a))
  -> Relations a
  -> t
  -> t1
  -> ReductionMode
  -> IO (Relations a, Relations a, Relations a, Invs (CPU a), EdgeCount, EdgeCount)

accAnalysis cpu fix rel prog instrs NotReduced
  = do
    (startLabel, invsS, _, cfg', initialSt, rel',_) <- defaultCFG fix cpu rel
    case fix of
         Nothing -> do
                   ((meta, logs), st) <- runStateT
                                         (runWriterT
                                         (runReaderT (generator Supplier return cfg') Nothing))
                                         initial
                   processGraphML logs

                   out@St { invs = cert, edges = le } <- meta initialSt

                   return (rel', rel', rel', cert, le, le)

         Just _  -> do
                   ((meta, logs), st) <- runStateT
                                           (runWriterT
                                           (runReaderT (generator Consumer return cfg') Nothing ))
                                           initial

                   out@St { invs = cert, edges = le }
                        <- meta $ initialSt { invs = resetCert cpu (ipoint startLabel) invsS }

                   return (rel', rel', rel', cert, le, le)

accAnalysis cpu fix rel prog instrs Reduced
  = do
    (startLabel, invsS, iterationsS, cfg', initialSt, rel', _) <- defaultCFG fix cpu rel
    case fix of
         Nothing -> do ((t, logs1), st) <- runStateT
                                               (runWriterT
                                               (runReaderT
                                               (generator Supplier return cfg' ) Nothing ))
                                               initial

                       out@St { invs = cert, edges = iterationsS' } <- --tic "CERT" $
                                                                       t initialSt

                       cfg'' <- evalStateT (transform cfg') (TransformState { count = iterationsS' , traceT = False}) -- iterationsS'

                       ((t', logs2), st) <- runStateT
                                                (runWriterT
                                                (runReaderT
                                                (generator Supplier return cfg'') Nothing))
                                                initial

                       let rel'' = (sort . toList) cfg''

                       out@St { invs = cert', edges = edgesS' } <- --tic "REDUCED" $
                                                                   t' initialSt

                       return (rel', rel'', rel'', cert', iterationsS', edgesS' )

         Just _ -> do  cfg_ <- evalStateT (transform cfg') (TransformState { count = iterationsS, traceT = False})

                       let cfg'' = (normalize . transformACC) cfg_

                       ((t', logs), st) <- runStateT
                                                (runWriterT
                                                (runReaderT
                                                (generator Consumer return cfg'') Nothing))
                                                initial

                       let rel'' = toList cfg_

                       out@St { invs = cert', edges = edgesS' }
                           <- --tic "VERIFY REDUCED" $
                              t' initialSt { invs = resetCert cpu (ipoint startLabel) invsS }

                       return (rel', rel'', rel'', cert', iterationsS, edgesS')


transformACC
  :: (Show a)
  =>  CFG a -> CFG a

transformACC (Rec (Leaf b) graph)
  = EmptyGraph

transformACC (Seq graphA graphB)
  = Seq (transformACC graphA) (transformACC graphB)

transformACC a
  = a
