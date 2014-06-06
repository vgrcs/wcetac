-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Certificate
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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
module Analyzer.Certificate (
  Invs (..), ACC (..), NodeCount (..), EdgeCount (..), Node (..) , InLoop (..), Loop (..), Cert (..),
  Primal, Dual , InvsWrapper (..), Context (..), Stable (..),
  NodeCountWrapper (..), EdgeCountWrapper (..), Times (..), Redirects, Prop (..), Point (..)
) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Map
import GHC.Word
import System.CPUTime
import Text.Printf
import Data.Binary

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Label
import Analyzer.Lattice
import Arm.Instruction


-- | Program states are stored in program Nodes. The CPU is inside a IVar, booleans specify
--  fixpoint stabilization flags and InLoop stores for head of loops a list of node inside the loop.
data Node b = Node { value :: b,
                     stableFixpoint :: Bool,
                     stableValue  :: [Stable],
                     stableLoop :: Bool,
                     insideLoop :: InLoop,
                     contexts :: [Context],
                     redirect :: Maybe [Context],
                     infeasibleNode :: Bool }
              deriving Eq


instance (Lattice a) => Initialize (Node a) where
  initial =  Node { value = bottom, stableFixpoint = False, stableLoop = False,
                    stableValue = [], insideLoop = No,
                    contexts = [], redirect = Nothing, infeasibleNode = False }


data Stable = DataStable | MemStable | PipelineStable deriving (Eq, Show)


-- | Calling Contexts
type Context = Label -- Integer
type Redirects = Maybe [Context]

-- |
data InLoop = Yes [Label] | No deriving (Eq, Ord, Show)

-- | The certificate of program states is a map of invariants (Invs) of program points to Nodes
type Point = Integer
type Invs b = Map Point (Node b)
type Times b = Map Point [Prop b] -- [(Instruction, Word32, Redirects, b)]

data Prop b = Prop { syntax :: Instruction,
                     pc :: Word32,
                     connections :: Redirects,
                     prop :: b } deriving (Show, Eq)

-- |
data InvsWrapper a = InvsWrapper (Invs a)


-- | The other certificates result from the program flow analysis. Node and edges upper bounds.
type NodeCount = Map Integer Loop
type EdgeCount = Map (Integer, Integer) Loop

-- |
data NodeCountWrapper = NodeCountWrapper NodeCount
data EdgeCountWrapper = EdgeCountWrapper EdgeCount

instance Show NodeCountWrapper where
  show (NodeCountWrapper n) = unlines $ Prelude.map show (toList n)

instance Show EdgeCountWrapper where
  show (EdgeCountWrapper n) = unlines $ Prelude.map show (toList n)

-- | The maps containing the solutions of the ILP problem.
type Primal = Map Int Double
type Dual = Map Int Double


-- | The complete certificate also includes the cost.
data Cert b = Cert { invariants :: Invs b,
                     edgeCount :: EdgeCount,
                     iterations :: EdgeCount,
                     wcet :: Double,
                     primal :: Primal,
                     dual :: Dual }
              deriving (Eq)

-- | The program flow analysis domain is an order on natural numbers.
data Loop = TopL Int | BottomL deriving (Eq)

instance Num Loop where
  (TopL t)  + BottomL   = TopL t
  BottomL   + (TopL t)  = TopL t
  (TopL t1) + (TopL t2) = TopL $ t1 + t2
  fromInteger i = TopL (fromInteger i)

-- | Mode of the Abstraction Carrying Code
data ACC = Supplier | Consumer deriving (Eq)

-- |
instance Show Loop where
  show (TopL i) = show i ++ " times"
  show (BottomL) = "0 times"

-- |
instance Ord Loop where
  max (TopL a) (TopL b)  =  TopL (max a b)
  max (BottomL) (TopL b)  =  TopL b
  max (TopL a) (BottomL)   =  TopL a
  max (BottomL) (BottomL)   =  BottomL

-- |
instance Enum Loop where
  fromEnum (TopL x) = x
  fromEnum (BottomL ) = 0
  toEnum 0 = (BottomL)
  toEnum x = (TopL x)
  succ (TopL x) = (TopL (x+1))
  succ (BottomL ) = (TopL 1)
  --pred (BottomL) = predError "Loops:BottomL"
  pred (TopL x) = (TopL (x-1))


instance Lattice InLoop where
  bottom = No
  join a b = case (a,b) of
                  (No, No) -> No
                  (No, Yes a) -> (Yes a)
                  (Yes a, No) -> (Yes a)
                  (Yes a, Yes b) -> (Yes (a ++ b))

instance Lattice NodeCount where
  bottom = fromList [(0, TopL 1)]
  join a b = unionWith (\l1 l2 -> max l1 l2) a b


instance Lattice EdgeCount where
  bottom = empty
  join a b = unionWith (\l1 l2 -> max l1 l2) a b



{- tic :: String -> IO t -> IO t
tic str a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    --printf "Computation time: %0.3f sec\n" (diff :: Double)
    printf (str ++ " : %0.3f sec\n") (diff :: Double)
    return v -}
