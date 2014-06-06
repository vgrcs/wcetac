-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.LRU
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Vítor Rodrigues
-- Stability   :
-- Portability :
-- Email       :  vitor.gabriel.rodrigues@gmail.com
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Analyzer.LRU where

import Arm.Decoder

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Foldable
import Data.Word
import qualified Data.Sequence as Seq
import Data.List
import qualified Data.Vec as Vec
import System.IO.Unsafe
import Control.Monad


class LRUCache a where
   bottomLRU :: a
   mergeBoxLRU :: a -> a -> (Int -> Int -> Bool) -> ([Address] -> [Address] -> [Address]) -> a

type Address = Word32
type WordAddress = Address
type ByteAddress = Address

type CacheLine = (Address, OpCode)

type Cache = Seq.Seq [CacheLine]

data Class = Hit Int | Miss deriving (Show)

data OpCode = OpCode Word64
            | BottomW deriving (Eq, Ord)

instance Show OpCode where
         show (OpCode a) =  show a
         show BottomW  = "_|_"


data CacheLevel = L1 | L2 deriving Show
cacheSize L1 = 1024
cacheSize L2 = 0
emptyCache level = Seq.fromList (List.take (cacheSize level) (List.repeat []) )

data L2Box = L2Box Cache deriving (Ord, Eq)
data L1Box = L1Box Cache deriving (Ord, Eq)


{-instance Eq L1Box where
  L1Box a == L1Box b  = filter (\ c -> not $ null c) (toList a) == filter (\c -> not $ null c) (toList b)

instance Eq L2Box where
  L2Box a == L2Box b  = filter (\ c -> not $ null c) (toList a) == filter (\c -> not $ null c) (toList b)
-}

instance Show L1Box where
  show (L1Box l) =  show (CacheWrapper l)

instance Show L2Box where
  show (L2Box l) =  show (CacheWrapper l)

instance LRUCache L1Box where
         bottomLRU = (L1Box $ emptyCache L1)
         mergeBoxLRU a b age op
           = let L1Box ca  = a
                 L1Box cb  = b
                 c' = unsafePerformIO $ mergeLRU ca cb age op
             in (L1Box c')

instance LRUCache L2Box where
         bottomLRU = (L2Box $ emptyCache L2)
         mergeBoxLRU a b age op
            = let L2Box ca = a
                  L2Box cb = b
                  c' = unsafePerformIO $ mergeLRU ca cb age op
              in (L2Box c')


mergeLRU
  ::   Cache
      -> Cache
      -> (Int -> Int -> Bool)
      -> ([Address] -> [Address] -> [Address])
      -> IO Cache

mergeLRU cA cB age op
    = do
      let elemA = List.map (\ line -> List.map fst line) (toList cA)
          elemB = List.map (\ line -> List.map fst line) (toList cB)
          lA = toList cA
          lB = toList cB
          sA = filter (/= []) lA
          sB = filter (/= []) lB
          addrs = op (List.concat (toList elemA)) (List.concat (toList elemB))
          f d c addr = do
                     let lineA = access cA addr
                         lineB = access cB addr
                     --putStrLn ("ADDR: " ++ show addr ++ " in A: " ++ show lineA ++ " in B: " ++ show lineB)
                     case (lineA, lineB) of
                          (Hit la, Hit lb) ->
                                 if la /= lb
                                    then do
                                         c' <- updateCacheAt d addr la lb age c cA cB
                                         --putStrLn $ "NEW= " ++ show (CacheWrapper c')
                                         return c'
                                    else do
                                         let c' = Seq.adjust nub la c
                                         return c'
                          (Hit l, Miss) -> return c
                          (Miss, Hit l) -> return c
                          (Miss, Miss) -> return c
          c = if sB > sA
                 then Seq.fromList (List.zipWith (++) lB lA)
                 else Seq.fromList (List.zipWith (++) lA lB)
          --c = Seq.fromList (List.zipWith (++) lA lB)

      --putStrLn $ "join caches= " ++ show addrs
      --putStrLn $ "elems A= " ++ show (List.concat (toList elemA))
      --putStrLn $ "elems B= " ++ show (List.concat (toList elemB))
      let debug = False
#if defined(SIM)
      {-debug  <- do
                putStrLn "LRU"
                cmd <- getLine
                putStrLn ""
                case List.head cmd of
                     'a' -> do
                           putStrLn $ "A= " ++ show (CacheWrapper cA)
                           putStrLn $ "B= " ++ show (CacheWrapper cB)
                           putStrLn $ "C= " ++ show (CacheWrapper c)
                           return True
                     _ -> return False-}
#endif
      --putStrLn $ "A= " ++ show (CacheWrapper cA)
      --putStrLn $ "B= " ++ show (CacheWrapper cB)
      --putStrLn $ "C= " ++ show (CacheWrapper c)
      c' <- foldM (f debug) c addrs

      let c'' = filter (\l -> not $ List.null l) $ toList c'
          diff = Seq.length c' - List.length c''
          c_ = Seq.fromList (c''  ++ (replicate diff [] ) )


      --if c_ /= c'
      --   then error $ "W " ++ show diff
      --   else return ()
#if defined(SIM)
      --putStrLn $ "C'= " ++ show (CacheWrapper c_)
#endif
      return c_


data CacheWrapper = CacheWrapper Cache
data CacheLineWrapper = CacheLineWrapper CacheLine

instance Show CacheWrapper where
   show (CacheWrapper c)
     =  let  --c' = filter (\x -> not $ null x) (toList c)
             c' = toList c
             ixs = [0 .. (length c') - 1]
             c'' = zip ixs c'
             c''' = filter (\ (i,set) -> set /= []) c''
             cache = List.map (\(i, set) -> show i ++ " => " ++ showLine set) c'''
             --cache = List.map (\set -> showLine set)  (toList c)
             showLine [] = ""
             showLine set = show $ List.map (show . CacheLineWrapper) set
             --cache' = filter (/= "") cache
        in  unlines cache


instance Show CacheLineWrapper where
  show (CacheLineWrapper elem)
    =  case elem of
            (a, OpCode op) -> let Just i = Arm.Decoder.decode op
                             in formatStr2 35 ' ' ("[" ++ show (4*a) ++ "] " ++ show i)
            (a, BottomW)  -> "[" ++ show (4*a) ++ "] " ++ "Nothing"


formatStr2 places char str
  = let pad = places - (length str)
        fill = (take pad (repeat char))
    in [' '] ++ str ++ fill

access
  ::  Cache
      -> WordAddress
      -> Class

access cache addr
 = let ix = findIndex (hit addr) (toList cache)
    in case ix of
            Nothing -> Miss
            Just i -> Hit i

hit
  ::  WordAddress
      -> [CacheLine]
      -> Bool

hit addr coll
  =  let  map = Map.fromList coll
          addrs = Map.keys map
     in  List.elem addr addrs


updateCacheAt
   ::  Bool
       -> Address
       -> Int
       -> Int
       -> (Int -> Int -> Bool)
       -> Cache -> Cache -> Cache
       -> IO Cache

updateCacheAt d addr la lb age cache _ _
   =  do
      let  (line, index) = if la == lb
                              then (Seq.index cache la, la)
                           else if lb `age` la
                              then (Seq.index cache lb, lb)
                           else if la `age` lb
                              then (Seq.index cache la, la)
                           else error "comparing ages"

           pos = findIndex (\(a,_) -> a == addr) line
      --putStrLn $ "position of " ++ show addr ++ " in line " ++ show (map CacheLineWrapper line) ++ " is " ++ show pos
      --putStrLn $ "index of updated line " ++ show index
      let  (line', content )
               = case pos of
                      Just p -> let c = line !! p
                                in (List.delete c line, c)

      return $ Seq.update index line' cache



