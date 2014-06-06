
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ILP
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
{-# LANGUAGE NamedFieldPuns #-}
module Analyzer.ILP ( calculation, maxCountAt, processCallingContexts, convert

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Vec as Vec
import Data.List hiding (group)
import Data.Maybe
import Data.Word
import Control.Monad.Reader
import Control.Exception
import System.IO.Unsafe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics hiding (Expr, Cons)
import Analyzer.Label
import Analyzer.Certificate
import Analyzer.Analysis
import Analyzer.PipelineModel
import Analyzer.Lattice
import Arm.CPU
import Arm.Pipeline
import Arm.Instruction
import Data.LinearProgram hiding (var)


data ILP_rel = None | Cons Int | ILPNode Int | CountEdge Int | PlusILP ILP_rel ILP_rel | EqualILP ILP_rel ILP_rel  deriving (Eq)

instance Show ILP_rel where
	   showsPrec _ (CountEdge  d)    = if d < 0 then (("(-d" ++ (show (abs d))++")") ++) else (("d" ++ (show d)) ++)
	   showsPrec _ (ILPNode  x)    = (("x" ++ (show x)) ++)
	   showsPrec _ (Cons  l)    =  ((show l) ++)
	   showsPrec _ (PlusILP a b)  =  ((show a ++ " + " ++ show b) ++)
 	   showsPrec _ (EqualILP a b) =  ((show a ++ " = " ++ show b) ++)
 	   showsPrec _ (None) =  (("None") ++)

data Expr = BoolExpr Bool | Double Double | Equal Expr Expr | Plus Expr Expr deriving (Eq, Show)


calculation
  :: (Ord t, Cost t, Show t, Stateable a, Transition (Rel a), Ord (Coord t)) => [Rel a]
  -- -> Invs (CPU t)
  -> Times t
  -> NodeCount
  -> ReductionMode
  -> ACC
  -> Maybe (Double, Primal, Dual)
  -> IO [(Bool, Double, Primal, Dual)]

calculation rel acc edges mode side solutions
 = do
   let nodes = (foldl (\n r -> let (to, from) = ppoints r
                                   in n ++ ((fromInteger.ppoint) to):
                                          ((fromInteger.ppoint) from):[]) [] rel)
       nodes' = sort $ [-1] ++ (filter (>= 0) nodes)
       rel' = foldl (\l r -> l ++ (r, elemIndex r rel):[]) [] rel

   let calcproc accum n
          = do
            eqs <- equations n rel' []
            case side of
                 Supplier -> do
                            ret  <-  glpSolveVars simplexDefaults "default.lp"
                                     (lp (nub eqs) (nub n) rel acc edges False)
                            let (returncode, Just (wcet, tuples, vars, values, dual_values)) = ret
                            return (accum ++ (False, wcet, values, dual_values):[])
                 Consumer->  do
                            let (wcet, dual, primal) = fromJust solutions
                            verf <- verify (nub eqs) (nub n) rel acc edges (wcet, dual, primal)
                            return (accum ++ (verf, wcet, dual, primal):[])

   foldM calcproc [] [nodes']

int_ x = case abs ((abs x) - (abs ((fromIntegral . round . realToFrac) x))) <= 10^^(-5) of
              True -> True
              False -> error $ "not integer " ++ (show x)

equations
  :: Stateable a => [Int]
   -> [(Rel a, Maybe Int)]
   -> [ILP_rel]
   -> IO [ILP_rel]

equations [] _ ilp
  = return $ filter (\i -> i /= None) ilp

equations (n:ns) rel ilp
  = do
    let incoming n (r,_) = if n == -1
                              then n == (fromInteger . ppoint . sink) r &&
                                   (procName . parent . sink) r == "main"
                              else n == (fromInteger . ppoint . sink) r
        outcoming n (r,_) = n == (fromInteger.ppoint. source) r

        inx = findIndices (incoming n) rel
        outx = findIndices (outcoming n) rel
        sum [i] = let (_, Just edge) = rel !! i in (CountEdge edge)
        sum (i:is) = case is of
                          [] -> CountEdge i
                          _ -> PlusILP (CountEdge i) (sum is)
        eq = case (inx, outx) of
                  ([], []) -> None:[]
                  ([],_) -> (EqualILP (ILPNode n) (sum outx)):[]
                  (_,[]) -> (EqualILP (ILPNode n) (sum inx)):[]
                  (_,_) -> (EqualILP (ILPNode n) (sum inx)):
                           (EqualILP (ILPNode n) (sum outx)):[]
    equations ns rel (ilp ++ eq)


verify ::
   (Ord t, Cost t, Show t, Stateable a, Ord (Coord t))  =>[ILP_rel]
  -> [Int]
  -> [Rel a]
  -- -> Invs (CPU t)
  -> Times t
  -> NodeCount
  -> (Double, Primal, Dual)
  -> IO Bool

verify eqs nodes rel acc edges (wcet, primal, dual)
  =  do

     (returncode2, Just (m,n,mat, bounds, coefs, vars)) -- rows * column
           <- --tic "GET" $
              glpSolveMatrix simplexDefaults  (lp eqs nodes rel acc edges True)

     let axx = map (\(row, coefs) ->
                      let v = foldl (\accum (ix, c) ->
                                       let x = primal Map.! ix
                                       in accum ++ (x:[])
                                    ) [] coefs
                      in (row, v)
                  ) mat

     let ax = map (\(row, coefs) ->
                      let v = foldl (\accum (ix, c) ->
                                       let x = primal Map.! ix
                                       in accum + x*c
                                    ) 0 coefs
                      in (row, v)
                  ) mat

     let column n = map (\(row, coefs) ->
                            let i = findIndex (\(c,v) -> c == n) coefs
                            in case i of
                                    Just ix -> let (_,v) = coefs !! ix
                                              in (row, v)
                                    Nothing -> (row, 0.0)
                        ) mat

     let yss = map column [1..n]

     let ya = map (\c -> let col = column c
                        in foldl (\accum (ix, c) -> let (_,x) = col !! (ix-1)
                                                   in accum + (x*c)
                                 ) 0 (Map.toList dual)
                  ) [1..n]


     let solutionP = and (zipWith (\(i, bounds) (j, verf) ->
                                      case bounds of
                                           Equ b -> verf == b
                                           UBound b -> verf <= b
                                  ) bounds ax)

     let solutionD = and (zipWith (\(i, coef) (verf) -> let  verf' = fromIntegral (round (realToFrac verf))
                                                             b = verf' >= coef
                                                       in b
                         ) coefs ya)

     let primalDualEqual = let cx = zipWith (\(i,c) (j,x) -> c*x) coefs (Map.toList primal)
                               yb = zipWith (\(i,y) (j,b) -> case b of
                                                                 Equ b' -> y * b'
                                                                 UBound b' -> y * b'
                                            ) (Map.toList dual) bounds
                               cx' =   (fromIntegral . round . realToFrac) (sum cx)
                               yb' =   (fromIntegral . round . realToFrac) (sum yb)
                           in  (fromIntegral . round . realToFrac) (sum cx) ==
                               (fromIntegral . round . realToFrac) (sum yb)

     let sameWCET = let cx = zipWith (\(i,c) (j,x) -> c*x) coefs (Map.toList primal)
                    in sum cx == wcet

     return $ solutionD && solutionP && primalDualEqual && sameWCET


negateN (ILPNode i) = ILPNode (negate i)

edgeList :: ILP_rel -> [ILP_rel]
edgeList r = let f l (PlusILP p1 p2)  = f l p1 ++ f l p2
                 f l (CountEdge label) = l ++ (CountEdge label):[]
                 f l (Cons _) = l
             in f [] r


maxAlgebra :: (Cost a, Show a) => [Word32] -> [Instruction] -> [Prop a] -> Bool -> Int -> IO Int
maxAlgebra pcs is costs dbg accum
  =  do
     let costs_ = filter select costs where select p = elem (pc p) pcs
         rule a prop@Prop { syntax = i, prop = p }  = return $ (max a (relative p))

     foldM rule accum costs_

plusAlgebra :: (Cost a, Show a) => [Word32] -> [Instruction] -> [Prop a] -> Bool -> Int -> IO Int
plusAlgebra pcs [] costs dbg accum
  = return accum

plusAlgebra pcs is costs dbg accum
  =  do
     let locals i = filter select costs where select p = i == (syntax p)
         rdts = foldl collect Nothing where collect a p = mplus a (connections p)

     let pairs = map (\p@Prop { syntax, pc } -> (syntax , pc) ) costs
         local (i, pc') = filter select costs where select p = (pc p) == pc'
         collect a p = max a ((relative . prop) p)
         same is =  foldl collect 0 is
         tuple (i, pc') = return $ (i, pc', same (local (i, pc'))):[]

     let f c (i, pc') = do
                        t <- tuple (i, pc')
                        return $ nubBy (\(_,a,_) (_, b,_) -> a == b) $ c ++ t

     costs__ <- foldM f [] pairs

     let rule a (i, _, p) = return $ a + p

     foldM rule accum costs__


data CollectTimes t a = CollectTimes { node :: Integer,
                                       returns :: [(Integer, Integer)],
                                       times :: Times a,
                                       incomings :: [Rel t],
                                       outgoings :: [Rel t],
                                       arrivings :: [Rel t] }

group :: (Stateable t) => [Rel t] -> [[Instruction]]
group rs = map (toListExpr . expr) rs

serialize :: (Stateable t) => [Rel t] -> [Instruction]
serialize is = concat $ group is

rdt :: Integer -> [(Integer, Integer)] -> Bool
rdt point rdts  = point `elem` (snd . unzip) rdts

from :: Integer -> [(Integer, Integer)] -> Bool
from point rdts = isJust $ lookup point rdts

maxCountAt
  :: (Cost a, Show a, Stateable t) => [Rel t]
     -> Times a
     -> Map.Map Integer [(Instruction, Word32)]
     -> [(Integer, Integer)]
     -> Integer
     -> IO (Int, Int)

maxCountAt rel invariants exits rdts at
  = do
    let incomingToNode   = filter (\ r -> (ppoint.sink) r == at) rel
        outgoingFromNode = filter (\ r -> (ppoint.source) r == at) rel
        incomingToHook   = filter (\ r -> (ppoint.source) r `elem` rdt &&
                                          (ppoint.sink) r == -1 ) rel
                           where rdt = map (toInteger. snd) $
                                  filter (\r -> fst r == at) rdts


    let completeSum = length (serialize incomingToHook) >= 1  &&
                      (from at rdts)
        partialSum  = length (serialize outgoingFromNode) > 1 &&
                      not (rdt at rdts) && not (from at rdts)
        maxs        = not (from at rdts) && (length incomingToNode >= 1)

    let reader = CollectTimes { node = at, returns = rdts, times = invariants,
                                incomings = incomingToNode, outgoings = outgoingFromNode,
                                arrivings = incomingToHook }
    let f k n acc = unsafePerformIO $ runReaderT (collect k n acc) reader
    let count = Map.foldrWithKey f 0 invariants

    return (fromInteger at, count)



collect :: (Cost a, Show a, Stateable t) =>
           Integer -> [Prop a] -> Int  -> ReaderT (CollectTimes t a) IO Int
collect k _ accum
  = do
    c@CollectTimes { node = at, times = invariants } <- ask
    (maxI, pcM) <- maxs
    (sumI, pcS) <- sums

    if k == toInteger at
       then return $ unsafePerformIO $ do
            let times = concat $ Map.elems invariants
                select counters prop = elem (pc prop) counters

                costsM = filter (select pcM) times
                costsS = filter (select pcS) times
            maxAlgebra pcM maxI costsM False accum >>= plusAlgebra pcS sumI costsS False
       else return $ accum


maxs :: (Stateable t) => ReaderT (CollectTimes t a) IO ([Instruction], [Word32])
maxs = do
       reader@CollectTimes { node, returns, incomings, arrivings } <- ask

       let this = fromIntegral $ max 0 (node * 4)
           lastInstr = \is -> map (last . toListExpr . expr) is
           injects  = if from node returns then lastInstr arrivings else lastInstr incomings

       return $  if not (from node returns) && (length incomings >= 1)
                    then if node == 0 then (injects, []) else (injects, this:[])
                    else ([],[])


sums :: (Stateable t) => ReaderT (CollectTimes t a) IO ([Instruction], [Word32])
sums = do
       reader@CollectTimes { node, returns, incomings, outgoings, arrivings } <- ask

       let this = fromIntegral $ max 0 (node * 4)
           drop = \is -> take ((length is) - 1) is
           sizeOne = \seq -> (length seq == 0 && from node returns) ||
                             and (map (\is -> length is == 1) (group seq))

           completeSum = length (serialize arrivings) >= 1 && (from node returns)
           partialSum  = length (serialize outgoings) > 1 &&
                         not (rdt node returns) && not (from node returns)

           sufixes  = concat $ map drop $ group outgoings
           inserts  = if node == -1
                         then serialize incomings
                         else assert (from node returns) $ serialize arrivings

       return $
        if completeSum
        then let entering = let first = ((fromIntegral . ppoint . source . head) arrivings) * 4
                                limit = first + ((length (serialize arrivings) - 1) * 4)
                                pcs   = this:[first+4, first+8 .. limit]
                            in (inserts, map fromIntegral pcs)
             in if node == -1 || rdt node returns -- direct to exit
                   then entering
                   else let first = 4 + ((fromIntegral . ppoint . source . head) outgoings) * 4
                            limit = first + ((length (serialize outgoings) - 2) * 4)
                            pcs   = [first, first+4 .. limit]
                        in (fst entering ++ sufixes, snd entering ++ map fromIntegral pcs)

        else if partialSum
                then let size  = if length outgoings == 1
                                     then length (serialize outgoings)
                                     else assert (sizeOne outgoings) 1
                         limit = this + (size - 1) * 4
                         pcs   = if size == 1 then [] else [this+4, this+8 .. limit]
                     in (sufixes, map fromIntegral pcs)
        else ([], [])


lp
  :: (Cost t, Ord t, Show t, Transition (Rel a),  Stateable a, Ord (Coord t)) => [ILP_rel]
  -> [Int]
  -> [Rel a]
  -> Times t
  -> NodeCount
  -> Bool
  -> (LP String Int)

lp eqs nodes rel invariants edges consumer
  = do
    let  f_edge accum (ILPNode x) = accum ++ (-1, "x" ++ show (x)):[]
         f_edge accum (CountEdge e) = accum ++ (1, "d" ++ show e):[]
         start_edge = equalTo (linCombination (f_edge [] (CountEdge (0)))) 1
         f_equation ilp = let EqualILP ilp1 ilp2 = ilp
                              es = edgeList ilp2 ++ (ilp1):[]
                          in linCombination (foldl f_edge [] es)
         equations = map f_equation eqs

         (rdts, instrsToExits) =  processCallingContexts invariants
         info = unsafePerformIO $ mapM (maxCountAt rel invariants instrsToExits rdts)
                                  (map toInteger nodes)

         f_objective (node, max_cycles) = let var = "x" ++ show node
                                          in (max_cycles, var)

         objFun = linCombination (map f_objective info)

    execLPM $ do  setDirection Max
                  setObjective objFun
                  let bound c accum e = accum >> (leqTo (varSum ["d"++show e]) c)

                  let constraints_eqs s a
                         = foldl (\cstr lc -> cstr >> (equalTo lc 0)) s a

                      constraints_bounds s a
                         = foldl (\cstr (ed, count) ->
                                    (bound (fromEnum count)) cstr ed
                                 ) s a

                  constraints_eqs start_edge equations
                  let (e,c):es = Map.toList edges
                  constraints_bounds (leqTo (varSum ["d"++show e]) (fromEnum c)) es -- (Map.toList edges)

processCallingContexts
  :: (Show a) => Times a
  ->  ([(Integer, Integer)], Map.Map Integer [(Instruction, Word32)])

processCallingContexts invariants
  = let (exits, rdts)
            = mapAccumL (\accum (at, times) ->
                          let hooks = foldl (\accum_ prop@Prop { syntax = i, connections = r } ->
                                              case r of
                                                   Just [] -> nub $ accum_ ++ [initial] --[-1]
                                                   Just rs -> nub $ accum_ ++ rs
                                                   Nothing -> accum_
                                            ) [] times
                              maps = foldl (\a h -> a ++ (ppoint h,at):[] ) [] hooks
                          in (nub (accum ++ hooks), maps)
                        ) []  (Map.assocs invariants)

        instrsToExits
            = Map.filter (not . null) $
              Map.mapWithKey (\at times  ->
                                 if elem at (map ppoint exits)
                                    then let f a p = nub $ a ++ (syntax p, pc p):[]
                                         in  foldl f [] times
                                    else []
                             ) invariants

        rdts_ = concat $ filter (\r -> not (null r)) rdts

    in (rdts_, instrsToExits)


convert
  :: (Show a, Eq a) =>
     Integer
  ->  (Node (CPU a))
  ->  IO [ Prop a ]

convert at n@Node {value = BottomCPU,  redirect}
  = return []

convert at n@Node {value = cpu,  redirect}
  = do
    let core@Core {pipeline}  = multi cpu Map.! snd (active cpu)
    let convertPS at rdts p@PState { coords = Coord c }
          = let  f t@AbsTaskState { task = Done st }
                     = (taskInstr st, taskNextPc st, True)
                 f _ = (Nop, 0, False)

                 rdts_ = case rdts of { Nothing -> Nothing; Just (r:rs) -> Just (r:[]) ; r -> r }

                 g v a [s1,s2,s3]
                     = let  v1 = Vec.getElem 0 v
                            v2 = Vec.getElem 1 v
                            v3 = Vec.getElem 2 v
                       in case (third s1, third s2, third s3) of
                               (False, False,False) -> a
                               (False, False,True)  -> (instantiate s3 v3):a
                               (False, True, False) -> (instantiate s2 v2):a
                               (True, False, False) -> (instantiate s1 v1):a
                          where first  (c,_,_) = c
                                second (_,c,_) = c
                                third  (_,_,c) = c
                                instantiate s p = Prop { syntax = first s,
                                                         pc = second s,
                                                         connections = rdts_,
                                                         prop = property p }

                 bs = Vec.map f c
            in (g c) [] (Vec.toList bs)
    return $ concat $ nub $ Data.List.map (convertPS at redirect) pipeline


