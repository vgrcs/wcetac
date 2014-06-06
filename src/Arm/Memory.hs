
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Lattice
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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

module Arm.Memory ( Memory (..), SharedMemory (..), Main (..),
                    Classification (..), Stacked (..) , DataMem (..),
                    readMemInstrWord, writeMemWord, readMemWord, stackSize, writeMemInstrWord )
where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Array as Array
import Data.Word
import Data.Array
import Data.Maybe
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vec as Vec
import Control.Exception
import Data.Number.PartialOrd

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Register
import Arm.Decoder
import Analyzer.LRU
import Analyzer.Stack
import Analyzer.Lattice
import Analyzer.ValueAbstraction



-- | The instruction Memory CPO
data Memory = Memory  { must :: Main
                        , mustL1 :: L1Box
                        , mustL2 :: L2Box }
            deriving (Eq)

-- | The shared data memory
data SharedMemory
  = SharedMemory { datam :: DataMem ,
                   stack  :: Stack Stacked } deriving (Eq)


-- | Lattice instance for the instruction Memory structure
instance Lattice Memory where
  bottom
    = let --mustMain = (Array.listArray (0, 512) (repeat BottomW))
          mustMain = (Array.listArray (0, 4096) (repeat BottomW))
          mustL1_ =  bottomLRU
          mustL2_ =  bottomLRU
      in (Memory { must = mustMain, mustL1 = mustL1_, mustL2 = mustL2_ } )

  join a b
    = let  mustL1' = mergeBoxLRU (mustL1 a) (mustL1 b) (>) List.intersect
           mustL2' = mergeBoxLRU (mustL2 a) (mustL2 b) (>) List.intersect
      in (Memory {  must = must b, mustL1 = mustL1', mustL2 = mustL2' } )



-- | Lattice instance for the shared data Memory structure
instance Lattice SharedMemory where
  bottom
    = SharedMemory { datam = bottom, stack = emptyStack }
  join a b
    = let  datam' = join (datam a) (datam b)
           sta = Map.fromList (stackToList (stack a))
           stb = Map.fromList (stackToList (stack b))
           st' = Map.toList (Map.unionWith join sta stb)
           st'' = listToStack st'

      in (SharedMemory {  datam = datam' , stack = st'' } )


-- | Partial order on the instruction Memory domain
instance Ord Memory where
  compare a b
     = case (mustL2 a `compare` mustL2 b,  mustL1 a `compare` mustL1 b,  must a `compare` must b) of
                 (GT, _, _) -> GT
                 (_, GT, _) -> GT
                 (_, _, GT) -> GT
                 (EQ, EQ, EQ) -> EQ
                 _ -> LT

-- | Partial order on the data SharedMemory domain
instance Ord SharedMemory where
  compare a b = datam a `compare` datam b


-- | The data memory sub-component
data DataMem = DataMem (Array.Array Address Value) deriving (Show)

--type MemoryValue = (Value, Bool)

instance Eq DataMem where
  DataMem a == DataMem b
    =  Array.assocs a == Array.assocs b

-- | Lattice instance for the DataMem Memory component
instance Lattice DataMem where
  bottom
    = DataMem $ Array.listArray (0, stackSize)  (repeat Bottom)
  join (DataMem a) (DataMem b)
    = let ba = Array.bounds a
          bb = Array.bounds b
          m' = List.zipWith join (Array.elems a) (Array.elems b)
          m'' = zip (Array.indices a) m'
          indexes = assert (ba == bb) ba
      in ( DataMem (Array.array indexes m''))

instance Ord DataMem where
  compare (DataMem a) (DataMem b)
    = let a' = assocs a
          b' = assocs b
          m = List.zipWith (\(aa,va) (ab,vb) -> case le va vb  of
                                                    Just True -> True
                                                    Just False -> False
                                                    Nothing -> False
                           ) a' b'
     in if List.all id m
           then LT
           else if a == b then EQ
           else GT

-- | The instruction main memory sub-component
type Main = Array.Array Address OpCode


-- | The values inside the value are pairs address-value
type Stacked = (Address, Value)


-- | The cache access classification
data Classification = HR1 | HW1 | MR1 | MW1
                    | HR2 | HW2 | MR2 | MW2 | DontKnow
                    deriving (Eq, Show, Ord)

-- | Must and May classification
type Analysis = (Classification, Classification)


-- | Local addresses are managed using consecutive numbers
wordAddress
  :: ByteAddress
  -> WordAddress

wordAddress addr
   =  addr `div` 4

-- |  Get the data value from the Memory at the WordAddress
getMemWord
  :: SharedMemory
  -> WordAddress
  -> (Value, SharedMemory)

getMemWord mem addr
   = let  DataMem arr = datam mem
          val = (Array.!) arr addr
     in (val, mem)

-- | Get the instruction from the Memory at the WordAddress
getMemInstrWord
  :: WordAddress
  ->  Memory
  ->  IO (Analysis, OpCode, Memory)

getMemInstrWord addr mem@Memory{ must, mustL1, mustL2 }
   = do
     (a1, v1, mustL1', mustL2', must') <- getLRU addr must mustL1 mustL2
     let mem' = ( mem { must = must', mustL1 = mustL1' , mustL2 = mustL2' } )
     return ((a1, DontKnow), v1, mem')

-- | Write the data value (MemValue) into the Memory at the WordAddress
setMemWord
  :: WordAddress
  -> Value
  -> SharedMemory
  -> IO SharedMemory

setMemWord addr val mem@SharedMemory{ datam }
   =  if addr > stackSize
         then return mem
         else do
              let DataMem arr = datam
              let  prev = (Array.!) arr addr
              val' <- case val of
                           StdVal val -> return $ StdVal val
                           _ ->  do
                                 case cmp prev val of
                                      Nothing -> return prev
                                      _ -> return $ join  prev val
              let  datam' = DataMem ((Array.//) arr [(addr, val')])
              return (mem {datam = datam'})


-- | Write the instruction inside the domain OpCode into the Memory at the WordAddress
setMemInstrWord
  :: WordAddress
  -> OpCode
  -> Memory
  -> IO (Analysis, Memory)

setMemInstrWord  addr val mem@Memory{ must, mustL1, mustL2 }
   =  do
      (a1, mustL1', mustL2', must') <- setLRU addr val must mustL1 mustL2
      let mem' = ( mem { must = must' , mustL1 = mustL1' , mustL2 = mustL2' } )
      return ((a1, DontKnow), mem')

-- | Update the complete LRU cache upon a "set", including the levels L1 and L2
--   Return the classification
setLRU
  :: WordAddress
  ->  OpCode
  ->  Main
  ->  L1Box
  ->  L2Box
  ->  IO (Classification, L1Box, L2Box, Main)

setLRU addr val main (L1Box l1) (L2Box l2)
  =  do
     (a, o, l1', l2', m') <- updateLRU addr (Just val) L1 l1 l2 main Write
     return (a, L1Box l1', L2Box l2', m')

-- | Update the complete LRU cache upon a "get", including the levels L1 and L2
--   Return the classification
getLRU
  :: WordAddress
     -> Main
     -> L1Box
     -> L2Box
     -> IO (Classification, OpCode, L1Box, L2Box, Main)

getLRU addr main (L1Box l1) (L2Box l2)
  =  do
     (a, o, l1', l2', m') <- updateLRU addr Nothing L1 l1 l2 main Read
     return (a, o, L1Box l1', L2Box l2', m')


data ReadWrite = Read | Write deriving (Show)

-- | Generic "update" function for LRU caches. Modes are set by the ReadWrite flag.
--   Return the classification
updateLRU
  :: WordAddress
     -> Maybe OpCode
     -> CacheLevel
     -> Cache
     -> Cache
     -> Main
     -> ReadWrite
     -> IO (Classification, OpCode, Cache, Cache, Array WordAddress OpCode)

updateLRU addr input level l1 l2 m rw
  = do
    let
         cl = case level of
                   L1 -> access l1 addr
                   L2 -> access l2 addr
    (hm, output, l1', l2', m')
            <- case  cl of
                    Miss   -> do
                              case level of
                                    L1 -> do
                                         updateLRU addr input L2 l1 l2 m rw
                                    L2 -> do
                                          case rw of
                                               Write -> let Just a = input
                                                        in return (MW2, a, l1, l2, m)
                                               Read -> let val = (Array.!) m addr
                                                       in  return (MR2, val, l1, l2 , m)
                    Hit ix -> case rw of
                                   Read -> let  line = case level of
                                                            L1 -> Seq.index l1 ix --l1 Map.! ix
                                                            L2 -> Seq.index l2 ix --l2 Map.! ix
                                                map = Map.fromList line
                                                pos = Map.findIndex addr map
                                                (_, val) = Map.elemAt pos map
                                           in case level of
                                                       L1 -> return (HR1, val, l1, l2, m)
                                                       L2 -> return (HR2, val, l1, l2, m)
                                   Write -> let Just a = input
                                            in case level of
                                                    L1 -> return (HW1, a, l1, l2, m)
                                                    L2 -> return (HW2, a, l1, l2, m)

    (c'', e) <- case  level of
                      L1 -> updateCache l1' level addr output
                      L2 -> updateCache l2' level addr output

    let  m'' = case  e of
                     Nothing -> m'
                     Just coll -> evict m coll

    case level of
         L2 -> return (hm, output, l1', c'', m'' )
         L1 -> return (hm, output, c'', l2' , m'' )


-- | From outside read the data value from Memory
--   Unpack the address
readMemWord
  :: SharedMemory
  ->  Address
  ->  IO (Value, SharedMemory)

readMemWord mem byteAddr
   =  return $ getMemWord mem (wordAddress byteAddr)

-- | From outside read the instruction from Memory
--   Unpack the address
readMemInstrWord
   ::  Memory
       -> Address
       -> IO (Analysis, OpCode, Memory)

readMemInstrWord mem byteAddr
   = getMemInstrWord (wordAddress byteAddr) mem

-- | From outside write the data value from Memory
--   Unpack the address
writeMemInstrWord
  :: Memory
  ->  Address
  ->  OpCode
  ->  IO (Analysis,Memory)

writeMemInstrWord mem byteAddr val
   =  do
      let  addr = wordAddress byteAddr
      setMemInstrWord addr val mem

-- | From outside write the data value from Memory
--   Unpack the address
writeMemWord
  :: SharedMemory
  -> Address
  -> Value
  -> IO SharedMemory

writeMemWord mem byteAddr val
  = setMemWord (wordAddress byteAddr) val mem



-- | Update a specific level of the cache and return the evicted blocks
updateCache
  :: Cache
  ->  CacheLevel
  ->  WordAddress
  ->  OpCode
  ->  IO (Cache, Maybe (CacheLevel, [CacheLine]))

updateCache cache level addr opcode
   =  do
      let lgth = Data.Foldable.foldl (\ accum l -> if List.null l then accum else accum+1) 0 cache
          cSize = cacheSize level
      if  lgth < cSize
          then  do
                let shift = Seq.take (cSize-1) (removeAllCacheAt addr cache)
                    cache' = [(addr, opcode)] Seq.<| shift
                return (cache', Nothing)

          else if lgth == cSize && cSize /= 0
                   then  do let  evict = Seq.index cache (cSize-1)
                                 shift = Seq.take (cSize-1) (removeAllCacheAt addr cache)
                                 cache' = [(addr, opcode)] Seq.<| shift
                            return (cache', Just (level, evict))
          else if cSize == 0
                   then do
                        --putStrLn "cache size zero"
                        return (cache, Nothing)
          else  error ("top over size")

-- | Evict a set of memory blocks to the Main memory
evict
  :: Main
  ->  (CacheLevel, [CacheLine])
  ->  Main

evict m (_,[])
   =  m
evict m (level ,(c:coll))
   =  let  (addr', val') = c
           m' =  (Array.//) m [(addr', val')]
      in case level of
              L1 -> evict m (level, coll)
              _ -> evict m' (level, coll)


-- | Remove all the cached lines at WordAddress from the Cache
removeAllCacheAt
   :: (WordAddress)
   -> Cache
   -> Cache

removeAllCacheAt addr cache
   =  let cl = access cache addr
      in case cl of
              Hit i -> let line = Seq.index cache i
                           line' = List.filter (\(a,_) -> a /= addr) line
                       in  Seq.update i line' cache
              Miss -> cache

