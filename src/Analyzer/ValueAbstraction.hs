
-----------------------------------------------------------------------------
--
-- Module      :  ValueAbstraction
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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Analyzer.ValueAbstraction ( Value (..), Interval, Control (..), toInt32, min_value, max_value,
                                   disjoint, generate, showInterval, Stubs (..)

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Word
import Data.Bits
import Data.Map
import Data.Maybe
import Data.Number.PartialOrd


-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Lattice
import Arm.RegisterName

data Machine = Machine Word32 deriving (Eq, Show)

instance Num Machine where
  (Machine a) + (Machine b)
    = Machine (fromIntegral (toInt32 a + toInt32 b) :: Word32)
  (Machine a) - (Machine b)
    = Machine (fromIntegral (toInt32 a - toInt32 b) :: Word32)
  (Machine a) * (Machine b)
    = Machine (fromIntegral (toInt32 a * toInt32 b) :: Word32)
  abs (Machine a) = Machine (fromIntegral (abs (toInt32 a)) :: Word32)
  signum (Machine a) = if toInt32 a > 0 then 1 else if toInt32 a == 0 then 0 else -1
  fromInteger i = Machine (fromIntegral i :: Word32)

type Interval = (Word32, Word32)

data Value
  = RegVal Interval
  | BackVal Interval
  | MemVal (Interval, [Word32])
  | PtrVal Interval
  | PcVal  Interval
  | LrVal  Interval
  | SpVal  Interval
  | ConVal  Interval
  | CtrVal Control
  | StdVal Word32
  | CharVal Word32
  | Bottom
  deriving (Eq)


-- | Data type used during pipeling with temporary Registers
type Stubs = Map RegisterName Value

data Control = Control { control :: Word32,
                         lower :: Stubs,
                         lowersame :: Stubs,
                         lessthan :: Stubs,
                         equal :: Stubs,
                         greaterthan :: Stubs,
                         higher :: Stubs,
                         highersame :: Stubs,
                         back :: Stubs }
               -- deriving (Eq)

instance Eq Control where
  a == b = control a == control b

normalize (a, b) = if toInt32 a <= toInt32 b then (a, b) else (b, a)

instance Num Interval where
  (a,b) + (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
          (x, y) =  (max min_value (a' + c'), min max_value (b' + d'))
      in normalize (fromIntegral x :: Word32, fromIntegral y :: Word32)
  (a,b) - (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
          (x, y) =  (max min_value (a' - c'), min max_value (b' - d'))
      in normalize (fromIntegral x :: Word32, fromIntegral y :: Word32)
  (a,b) * (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
      in normalize
         ( fromIntegral (min (min (a' * c') (a' * d')) (min (b' * c') (b' * d'))) :: Word32,
           fromIntegral (max (max (a' * c') (a' * d')) (max (b' * c') (b' * d'))) :: Word32 )
  abs (a, b) = (abs a, abs b)
  fromInteger a = (fromIntegral a :: Word32, fromIntegral a :: Word32)
  signum (a,b) = error $ "signum not implemented for Interval"



instance PartialOrd Interval where
  cmp (a,b) (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
      in if  a' > c' && b' < d' then Just LT
             else if a' == c' && b' == d' then Just EQ
             else Just GT






instance Lattice Interval where
  bottom = error $ "bottom not implemented for Interval"
  join (a,b) (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
      in (fromIntegral (min a' c') :: Word32, fromIntegral (max b' d') :: Word32)
  meet (a,b) (c,d)
    = let (a', b') =  (toInt32 a, toInt32 b)
          (c', d') =  (toInt32 c, toInt32 d)
          l = max a' c'
          u = min b' d'
      in (fromIntegral l :: Word32, fromIntegral u :: Word32)



showInterval
  :: Interval
  ->  String
showInterval (a, b)
  = let (a', b') =  (toInt32 a, toInt32 b)
    in "[" ++ show a' ++ "," ++ show b' ++ "]"


disjoint
  :: Interval
  ->  Interval
  ->  Bool

disjoint (a,b) (c,d)
  = let (a', b') =  (toInt32 a, toInt32 b)
        (c', d') =  (toInt32 c, toInt32 d)
    in  max a' c' > min b' d'

generate
  :: Interval
  ->  [Integer]
generate (a,b)
  = let a' = toInt32 a
        b' = toInt32 b
    in [a' .. b']


-- | Limits for values, used in backward static analysis
max_value :: Integer
max_value = 2147483647

min_value :: Integer
min_value = -512


toInt32
  :: Word32
  ->  Integer
toInt32 w
  = let w' = fromIntegral w :: Integer
    in if w' > 2147483647
           then w' - 4294967296
           else w'
