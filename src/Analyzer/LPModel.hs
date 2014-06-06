-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.LPModel
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Analyzer.LPModel ( solveModelVars, verifyModel

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.List
import qualified Data.Map as Map
import System.IO.Unsafe
import Control.Exception

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Data.LinearProgram hiding (direction, objective, constraints)
import AMPL.AMPL
import Analyzer.Label
import Analyzer.Semantics
import Analyzer.Certificate
import Analyzer.ILP
import Arm.Pipeline

type Labels = String
type Nodes = String
type Edges = String


model (s :: Set Labels)
      (e :: Set Edges)
      (nodes :: Param_ Nodes)
      (cycles :: Param_ Nodes)
      (edges :: Param_ Edges)
      (bounds :: Param_ Edges)
  = do x <- var__ "x" s
       y <- var_ "d" e
       maximize "wcet" $ sum [cycles(j)*x(j) | j<-s]
       --error $ (show (x( s !! 7))) ++ "\n" ++  show (nodes ( s !! 7))
       --error $ (show (x( s !! 7))) ++ "\n" ++  show (edges ( e !! 5))
       --error $ (show [ x(i) %== sum [ nodes(i) | j<-e, edges(j) == Con (Pair (6,5)) ] | i <- [s!!7]])
       subject_to "incoming" [ x(i) %== sum [ y(j) | j<-e , nodes(i) %>> (edges(j)) ] | i <- s]
       subject_to "outgoing" [ x(i) %== sum [ y(j) | j<-e , nodes(i) %<< (edges(j)) ] | i <- s]
       subject_to "bounds" [ y(j) %<= bounds(j) | j<-e ]


checker2 (rs :: Set Int)
         (cs :: Set Int)
         (primal :: Solution_ Int)
         (dual :: Solution_ Int)
         (bounds :: Solution_ Int)
         (costs :: Solution_ Int)
         (rows :: RowSolution_ Int)
         (columns :: RowSolution_ Int)
         (wcet :: Solution)

  = let axb = and [ costs(i) <= (approx . sum) [ d*dual(j) | (j,d) <- c ]  | i <- cs, let c = columns (i) ]
        yxc = and [ bounds(i) >= sum [ c*primal (j) | (j,c) <- r ] | i <- rs, let r = rows(i) ]
        dp = sum [ primal(i)*costs(i)  | i <- cs] == (approx . sum) [ dual(i)*bounds(i)  | i <- rs]
        same = wcet == sum [ primal(i)*costs(i)  | i <- cs]
    in  axb && yxc && same && dp

approx = \x -> (fromIntegral . round . realToFrac) (int_ x)

int_ x = assert (abs ((abs x) - ((fromIntegral . round . realToFrac) x)) <= 10^^(-5)) x


checkerInstance rel invariants count rows columns matrix primal dual bounds coefs wcet
  = let primal_ = solution_ $ Map.toList primal
        dual_ = solution_ $ Map.toList dual
        bounds_ = solution_ $ map (\(i,b) -> case b of
                                      Equ b -> (i, b)
                                      UBound b -> (i, b)  ) bounds
        coefs_ = solution_ $ coefs

        column n = map (\(row, cs) ->
                            let i = findIndex (\(c,_) -> c == n) cs
                            in case i of
                                    Just ix -> (n, snd $ cs !! (ix))
                                    Nothing -> (n, 0.0)
                        ) matrix

        colInRow = let col n  = map (\(row, cs) ->
                                      let i = findIndex (\(c,_) -> c == n) cs
                                      in case i of
                                              Just ix -> (row, snd $ cs !! (ix))
                                              Nothing -> (row, 0.0) ) matrix
                   in zip [1..columns] (map col [1..columns])

        columns_ = row_ $ colInRow

        rows_ = row_ $ matrix

        cInstance = (checker2 [1..rows] [1..columns] primal_ dual_ bounds_ coefs_ rows_  columns_ wcet)
     in cInstance

modelInstance rel invariants count
  = let pairs = foldl (\a r -> if exit (sink r)
                                  then a ++ (pred (sink r), source r):[]
                                  else a ++ (sink r, source r):[]) [] rel
        states = concat $ map (\(a, b) -> a:b:[]) pairs
        instructions = foldl (\a r -> if False -- exit (sink r)
                                         then a
                                         else a ++ ((show . expr) r):[]) [] rel
        transitions = zipWith (\i n -> i++"_"++(show n)) instructions [0.. (length instructions)-1]
        exit sink = (procName . parent) sink /= "main" && (ppoint sink) == -1
        ids = sort $ nub $ delete (-2) (map (fromInteger . ppoint) states)
        labels = map (\i -> "node_" ++ show i) ids
        nodes = param_ (zip labels (map Val ids))
        id l = if (fromInteger . ppoint) l == -2
                  then NonComp
                  else (Val . fromInteger . ppoint) l
        edges = param_ (zip transitions (map (\(a, b) -> Pair (id a, id b) ) pairs))
        bounds = param_ (zip transitions (map (\(TopL b) -> Val b) (Map.elems count)))

        (rdts, instrsToExits) =  processCallingContexts invariants
        locals = unsafePerformIO $ mapM (maxCountAt rel invariants instrsToExits rdts)
                                   (map toInteger ids)
        cycles = param_ (zip labels (map (Val . snd) locals))

        mInstance = runModel (model labels transitions nodes cycles edges bounds)

        getEquations cs = (map fst $ filter (\c -> "incoming" ==  snd c) cs) ++
                          (map fst $ filter (\c -> "outgoing" ==  snd c) cs)
        getBounds cs = map fst $ filter (\c -> "bounds" ==  snd c) cs

        mergeEquations cs = let a = map fst $ filter (\c -> "incoming" ==  snd c) cs
                                b = map fst $ filter (\c -> "outgoing" ==  snd c) cs
                            in concat $ zipWith (\x y -> x:y:[]) a b

        x = (mergeEquations . constraints) mInstance
        y = zip ([1..length x]) x

    in --error (show ((getEquations . constraints) mInstance))
       --error (show ((mergeEquations . constraints) mInstance))
       --error (show count)
       --error ("ids= " ++ show (length ids))
       --error ("cycles= " ++ show (locals))
       --error ("edges= " ++ show (count))
       toGLPK ((fst . objective) mInstance)
              ((mergeEquations . constraints) mInstance)
              ((getBounds . constraints) mInstance)

solveModelVars
  :: (Show t, Cost t, Ord t, Transition (Rel a), Stateable a) => [(Rel a)]
  -> Times t
  -> NodeCount
  -> Double

solveModelVars rel invariants count
  = let lp = modelInstance rel invariants count
        solution = unsafePerformIO $ glpSolveVars simplexDefaults "new.lp" (runGLPK lp)
        (returncode, Just (wcet, tuples, vars, values, dual_values)) = solution
    in --error (show dual_values)
       wcet


verifyModel rel invariants count (wcet, primal, dual)
  = let lp = modelInstance rel invariants count
        solution = unsafePerformIO $ glpSolveMatrix simplexDefaults (runGLPK lp)
        (returncode2, Just (m,n,mat, bounds, coefs, vars)) = solution
        checker = checkerInstance rel invariants count m n mat primal dual bounds coefs wcet
    in --error (show (m,n)) --
       --(length mat, length bounds, Map.size primal, checker)
       (checker)


runGLPK :: LPM String Int () -> LP String Int
runGLPK lp = execLPM lp




{-lpModel
  :: (Cost t, Ord t, Show t, Transition (Rel a),  Stateable a, Ord (Coord t)) => [Int]
  -> [Rel a]
  -> Times t
  -> NodeCount
  -> (LP String Int)

lpModel nodes rel invariants edges
  = do
    let labels = foldl (\a r -> ((show . sink) r):((show . source) r):a ) [] rel
        transitions = foldl (\a r -> ((show . expr) r):a ) [] rel
    runGLPK $ testLP


runGLPK :: LPM String Int () -> LP String Int
runGLPK lp = execLPM lp

--translation
testLP :: LPM String Int ()
testLP = (leqTo (varSum ["dx"]) 3)
-}
