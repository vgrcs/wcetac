

-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Compiler
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
module Analyzer.Compiler ( generator

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.List hiding (break)
import Prelude hiding (break)
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State hiding (join, get)
import Control.Monad.Reader hiding (lift,join)
import System.IO.Unsafe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.IR
import Analyzer.MOP --hiding (debug)
import Analyzer.Operators
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.PipelineModel
import Analyzer.Certificate
import Analyzer.GraphMLBackend hiding (hook)
import Analyzer.Interpreter
import Analyzer.IR
import Analyzer.Interleavings
import ForSyDe.Backend.GraphML.AST
import Arm.CPU
import Arm.Pipeline
import Arm.Instruction


type FunTraces a = (Eval a) (a -> IO a)


generator
  :: (Show a, Cost a,  Ord a, FiveStagePipeline a, Show (Node (CPU a)), Show (Core a)) => ACC
     -> (St (CPU a) -> IO (St (CPU a)))
     -> CFG (St (CPU a))
     -> FunTraces (St (CPU a))



generator acc f (Leaf2 r)
  = do
    writeToGraph Normal (Just r)
    let g = (break2 r) **** (transf acc r) **** (continue2 r)
    --return $  f **** (transf acc r)
    return $ f **** g


generator acc f (Leaf a)
 = do
   writeToGraph Normal (Just a)
   return $  (f **** (transf acc a)  **** continue2 a)



generator acc f (Rec (Leaf t) graph)
  = do
    bid <- writeToGraph RecBegin (Just t)

    body <- generator acc return graph
    writeToGraph (RecEnd bid) Nothing
    return $ (f **** (body ++++ transf acc t))


generator acc f (Iter (Leaf t) graph)
  = do
    bid <- writeToGraph RecBegin (Just t)

    body <- generator acc return graph

    writeToGraph (RecEnd bid) Nothing
    return $ (f **** (body $$$$ transf acc t))


generator acc f (Choice c graphA graphB)
  = do
    let branch = transf acc c

    bid <- if  graphA == EmptyGraph
               then writeToGraph Break (Just c)
               else writeToGraph ChoiceBegin (Just c)

    --out <- liftIO $ basic2 graphA graphB
    --error "W"

    left  <- if graphA == EmptyGraph
                then generator acc break graphA
                else generator acc return graphA
    right <- generator acc return graphB
    writeToGraph (ChoiceEnd bid) Nothing

    return $ (f **** copy ****  ((branch **** left) //// (right)) **** wide c)

generator acc f (Seq graphA graphB)
  =  do
     as <- generator acc f graphA
     generator acc as graphB


generator acc f (Conc c a b)
  = do
    if False
       then  do
             let branch = transf acc c
             left <- generator acc return a
             right <- generator acc (create (toList b)) (colorfyG b)
             return $ (f **** copy ****  ((branch **** left) //// (right)) **** wide c)
       else do
            liftIO $ putStrLn $ "start"
            (i:intlvs) <- basic2 (Seq (Leaf c) a) b
            --error (show b)

            let   traces = zip [1.. (length intlvs)] (i:intlvs)
                  (traces', count) = enumerate i 0
                  traces'' = zip [1.. (length intlvs)] (map (\z -> fst (enumerate z 0)) (i:intlvs))
                  x = toList traces'
                  y = map (\r -> (expr r, (pos.sink) r, (pos.source) r )) x
            writeTraces  (take 1 traces'')
            --error (show (normalize i))
            detach
            is <- mapM (generator acc (create (toList b))) (map normalize (i:intlvs))  --((normalize i):[]) -- (i:intlvs)
            attach
            return $ f **** divide (length is) **** (multiply is) **** reduce
    where
    graphing t color = mapM_ (\r -> writeToGraph (InsideTrace t color) (Just r))
    writeTraces traces
      = do
        Just bid <- writeToGraph TraceSetBegin Nothing
        mapM_ (\(id, t) -> do
                           writeToGraph (TraceBegin id) Nothing
                           let trace = (nub) $ toList t
                               (entry, exit) = (head trace, last trace)
                           graphing id EnterThread (entry:[])
                           graphing id InsideThread $ (init . tail) trace
                           graphing id ExitThread (exit:[])
                           writeToGraph (TraceEnd id) Nothing
              ) traces
        writeToGraph (TraceSetEnd bid) Nothing


{-generator acc f (Conc c a b)
  = do
    Just bid <- writeToGraph InterleavingBegin Nothing
    intlvs <- liftIO $ interleavings a b

    let conc s@St { labelSt = at, invs=cert }
               = do
                 let new = unsafePerformIO $ do
                           putStrLn "###################################"
                           putStrLn $ " CONC :"
                           putStrLn "###################################"
                           return cert
                 return s { invs = new }

    let branch = transf acc c
    writeToGraph Normal (Just c)

    is <- mapM (generator acc (branch)) intlvs
    let is_ = concat $ map (\ (Right rs) -> rs) is
    let is'' = Left $ f **** divide (length is_) **** (multiply is_) **** reduce

    writeToGraph (InterLeavingEnd bid) Nothing

    return is''-}


generator _ f EmptyGraph = return return
-- generator _ _ other  = error ("generator " ++ show other)


attach :: (Eval a) ()
attach = modify (\ st@GeneratorSt {} -> st { trace = True } )

detach :: (Eval a) ()
detach = modify (\ st@GeneratorSt {} -> st { trace = False } )


debug :: String ->  (Eval a) ()
debug s = liftIO $ putStrLn $ s


basic = let c0 = ["a","b"]
            c1 = ["x","y"]
            f c p = c == filter ((flip elem) c) p
            p = [ is | is  <- permutations (c0++c1), f c0 is , f c1 is ]
        in p

basic2 :: (Stateable a, Initialize a) =>  CFG a -> CFG a ->  (Eval a) [CFG a]
basic2  main thread
  = do
    let f c p = c == filter ((flip elem) c) p
        --main_ = take 6 (toList main)
        main_ = toList main

        --main__ = Seq (Leaf (main_ !! 0)) (Leaf (main_ !! 1))
        --thread_ = take 2 $ map (colorfy InterleavedC) (toList thread)

        thread_ = map (colorfy InterleavedC) (toList thread)


        --thread__ = Seq (Leaf (thread_ !! 0)) (Leaf (thread_ !! 1))
        interleavings = [ is | is  <- permutations (main_ ++ thread_), f main_ is , f thread_ is ]

        break m r = if (isInterleaved . expr) r
                       then (m, (sink m, r):[])
                       else (r, [])

        groups is = map concat $
                    map (snd . (mapAccumL break initial)) is

        join gs (n, r)
                   = let (m, g):gs' = reverse gs -- (m, g) = last gs
                         in if n == m
                               then ((m, Seq g (Leaf2 r)):[]) ++ gs'
                               else (n, Leaf2 r):[] ++ gs
                               --else (n, EmptyGraph):[] ++ gs

        transf = foldl join ((initial, EmptyGraph):[])

        interleave m (l, g) = if l == initial
                                 then Seq g m
                                 else unsafePerformIO $ insertInterleave l m g

        tgroups =  (map transf) (groups interleavings)

    liftIO $ putStrLn $ "number of interleavings: " ++ show (length (interleavings))

    --liftIO $ putStrLn "MAIN"
    --liftIO $ mapM (putStrLn . show) ( main_ )
    --liftIO $ putStrLn "THREAD"
    --liftIO $ mapM (putStrLn . show) ( thread_ )
    --liftIO $ putStrLn (show (length interleavings))
    --liftIO $ putStrLn (show (length interleavings))

    let graphs = map (foldl interleave main) tgroups

    --liftIO $ mapM (putStrLn . show) ( interleavings )
    --error "W"

    --liftIO $ mapM (putStrLn . show) (take 3 (groups interleavings ))
    --error "T"
    --putStrLn "\n"
    --mapM (putStrLn . show) ((map transf) (groups interleavings))
    --putStrLn "\n"
    --putStrLn $ show thread
    --mapM (putStrLn . show) (graphs)
    --error (show ( (tagSync . normalize) (head graphs)))
    --return $ concat interleavings
    return $ map (normalize) graphs


_interleave_ m (l, g) = if l == initial
                         then Seq g m
                         else unsafePerformIO $ insertInterleave l m g


transformACC
  :: (Show a)
  =>  CFG a -> IO (CFG a)

transformACC (Rec (Leaf b) graph)
   = return EmptyGraph

transformACC (Seq graphA graphB)
  = do
    graphA' <- transformACC graphA
    graphB' <- transformACC graphB
    return $ Seq graphA' graphB'

transformACC a = return a



initialState rel label
   = initial { relational = rel, startLabel = label }

initialState2 rel start stop
   = initial { relational = rel, startLabel = start, stopLabel = stop, Analyzer.MOP.debug = True }
