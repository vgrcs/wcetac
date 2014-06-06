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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Arm.Register ( Interval, Std, Registers (..), Stubs (..), stackSize,
         cpsrSetByName, cpsrGetByName, setRegStub, getRegStub, fromValue,
         getControlLT, setControlLT, clearControlLT,
         getControlEQ, setControlEQ, clearControlEQ,
         getControlGT, setControlGT, clearControlGT,
         getControlB, setControlB, clearControlB,
         getControlC, setControlC, clearControlC,
         getControlI, setControlI, clearControlI,
         getControlLO, setControlLO, clearControlLO,
         getControlHI, setControlHI, clearControlHI,
         above, below, getReg, setReg, setReg',
         cpsrClearByName, showCPSRFlags,
         fromValueToInt32 ) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.Bits
import Data.Maybe
import Data.Int
import Control.Monad hiding (join)
import Data.Word
import Data.Array
import Data.List
import Data.Char hiding (Control)
import qualified Data.Array as Array
import qualified Data.Map as Map
import Control.Exception
import Data.Number.PartialOrd
import System.IO.Unsafe

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Lattice
import Analyzer.ValueAbstraction
import Arm.RegisterName


-- | The two types of values: intervals or standard simulation values
--type Interval = (Word32, Word32)
type Std = Word32


fromValue :: Value -> Interval
fromValue (RegVal i) = i
fromValue (BackVal i) = i
fromValue (PtrVal i) = i
fromValue (PcVal i) = i
fromValue (LrVal i) = i
fromValue (ConVal c) = c
fromValue (MemVal m) = fst m
fromValue (StdVal s) = (s,s)
fromValue Bottom = (0,0)
fromValue other = error $ "fromValue " ++ show other

fromValueToInt32 :: Value -> (Integer, Integer)
fromValueToInt32 (RegVal (a,b)) = (toInt32 a, toInt32 b)
fromValueToInt32 (BackVal (a,b)) = (toInt32 a, toInt32 b)
fromValueToInt32 (ConVal (a,b)) = (toInt32 a, toInt32 b)
fromValueToInt32 (MemVal ((a,b),_)) = (toInt32 a, toInt32 b)
fromValueToInt32 Bottom = (0, 0)
fromValueToInt32 other = error $ "fromValueToInt32 " ++ show other

-- |
stackSize :: Word32
stackSize = 1024


instance Num Value where
  Bottom + a = Bottom
  a + Bottom = Bottom
  RegVal a + RegVal b  = RegVal (a + b)
  PtrVal a + PtrVal b  = PtrVal (a + b)
  PcVal a + PcVal b    = PcVal (a + b)
  BackVal a + RegVal b  = RegVal (a + b)
  RegVal a + BackVal b  = RegVal (a + b)
  RegVal a + ConVal b  = RegVal (a + b)
  MemVal (a, x) + MemVal (b, y) = MemVal (a + b, nub (x ++ y))
  RegVal a + MemVal (b,y) = RegVal (a + b)
  MemVal (b,y) + RegVal a = RegVal (a + b)
  a + b = error $ "plus: " ++ show (a,b)

  Bottom - _ = Bottom
  _ - Bottom = Bottom
  RegVal a - RegVal b  = RegVal (a - b)
  BackVal a - BackVal b  = BackVal (a - b)
  PtrVal a - PtrVal b  = PtrVal (a - b)
  PcVal a - PcVal b    = PcVal (a - b)
  SpVal a - SpVal b    = SpVal (a - b)
  StdVal a - StdVal b    = StdVal (a - b)
  RegVal a - MemVal (b,y) = RegVal (a - b)
  MemVal (a, x) - MemVal (b, y) = MemVal (a - b, nub (x ++ y))
  a - b = error $ "minus: " ++ show (b)

  Bottom * _ = Bottom
  _ * Bottom = Bottom
  RegVal a * RegVal b  = RegVal (a * b)
  PtrVal a * PtrVal b  = PtrVal (a * b)
  PcVal a * PcVal b    = PcVal (a * b)
  SpVal a * SpVal b    = SpVal (a * b)
  MemVal (a, x) * MemVal (b, y) = MemVal (a * b, nub (x ++ y))
  MemVal (a, x) * RegVal (b) = RegVal (a * b)
  RegVal (a) * MemVal (b, y) = RegVal (a * b)
  a * b = error $ "mul: " ++ show (a,b)

  abs _ = error $ "abs not implemented for Value"
  signum _ = error $ "signum not implemented for Value"
  fromInteger _ = error $ "fromInteger not implemented for Value"


-- |
instance Show Value where
  show (RegVal i) =  "reg=" ++ showInterval i
  show (BackVal i) =  "back=" ++ showInterval i
  show (MemVal (i,a)) =  "mem=" ++ showInterval i ++ "." ++ show a
  show (ConVal i) =  "con=" ++ showInterval i
  show (PtrVal i) =  "ptr=" ++ showInterval i
  show (SpVal i) =  "sp=" ++ showInterval i
  show (PcVal i) =  "pc=" ++ showInterval i
  show (LrVal i) =  "lr=" ++ showInterval i
  show (CtrVal c) =  show c
  show Bottom  =  "_|_"
  show (CharVal v) = "str=[" ++ (wordToString v) ++ "]"
  show (StdVal v) = "std=[" ++ (show (toInt32 v)) ++ "]"


instance Show Control where
  show c =  let  cpsr = control c
                 lt' =  getControlLT cpsr
                 eq' =  getControlEQ cpsr
                 gt' =  getControlGT cpsr
                 lt  = lessthan c
                 eq  = equal c
                 gt  = greaterthan c
                 b   = getControlB cpsr
                 i   = getControlI cpsr
                 c_   = getControlC cpsr
            in ("\nLT=" ++ show (lt', lt) ++ "\nEQ=" ++ show (eq', eq) ++ "\nGT=" ++ show (gt', gt) ++
                "\nB=" ++ show b ++ ", I=" ++ show i ++ ", C=" ++ show c_ ++ "\nBACK=" ++ show (back c) )

wordToString
  :: Word32
  ->  String

wordToString word
  = do
    let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
    let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
    let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
    let c1 = fromIntegral (word .&. 0xFF)
    if c1 == 0
         then ""
         else if c2 == 0
                then [chr c1]
                else if c3 == 0
                       then [chr c1, chr c2]
                       else if c4 == 0
                              then [chr c1, chr c2, chr c3]
                              else [chr c1, chr c2, chr c3, chr c4]


instance PartialOrd Control where
  cmp a b = Just EQ

instance PartialOrd Value where
  cmp Bottom Bottom = Just EQ
  cmp Bottom _  =  Just LT
  cmp _ Bottom  =  Just GT
  CtrVal v1 `cmp` CtrVal v2 = v1 `cmp` v2

  RegVal v1 `cmp` RegVal v2 = v1 `cmp` v2
  BackVal v1 `cmp` RegVal v2 = v1 `cmp` v2
  RegVal v1 `cmp` BackVal v2 = v1 `cmp` v2
  BackVal v1 `cmp` BackVal v2 = v1 `cmp` v2


  PtrVal v1 `cmp` PtrVal v2 = v1 `cmp` v2
  SpVal v1 `cmp` SpVal v2 = v1 `cmp` v2
  LrVal v1 `cmp` LrVal v2 = v1 `cmp` v2
  PcVal v1 `cmp` PcVal v2 = v1 `cmp` v2

  ConVal v1 `cmp` ConVal v2 = v1 `cmp` v2
  ConVal v1 `cmp` RegVal v2 = v1 `cmp` v2
  RegVal v1 `cmp` ConVal v2 = v1 `cmp` v2

  StdVal v1 `cmp` StdVal v2 = if False then error ("different stds " ++ show (v1,v2)) else v1 `cmp` v2
  MemVal (v1,a1) `cmp` MemVal (v2,a2) = v1 `cmp` v2
  MemVal (v1,a1) `cmp` RegVal v2 = v1 `cmp` v2
  RegVal v1 `cmp` MemVal (v2,a2) = v1 `cmp` v2

  cmp a b = Nothing


instance Lattice Control where
  bottom = Control { control = 0,
                     lessthan = Map.empty, equal = Map.empty, greaterthan = Map.empty,
                     back = Map.empty, lower = Map.empty, higher = Map.empty,
                     lowersame = Map.empty, highersame = Map.empty }
  join a b = Control { control = let or = (control a) .|. (control b)
                                     and = (control a) .&. (control b)
                                     i = getControlI and
                                     or' = if i == 0 then clearControlI or else setControlI or
                                 in or',
                       lower = join (lower a) (lower b),
                       lowersame = join (lowersame a) (lowersame b),
                       higher = join (higher a) (higher b),
                       highersame = join (highersame a) (highersame b),
                       lessthan = join (lessthan a) (lessthan b),
                       equal = join (equal a) (equal b),
                       greaterthan = join (greaterthan a) (greaterthan b),
                       back = join (back a) (back b) }

  meet a b = Control { control = let or = (control a) .&. (control b)
                                     and = (control a) .&. (control b)
                                     i = getControlI and
                                     or' = if i == 0 then clearControlI or else setControlI or
                                 in or',
                       lower = meet (lower a) (lower b),
                       lowersame = meet (lowersame a) (lowersame b),
                       higher = meet (higher a) (higher b),
                       highersame = meet (highersame a) (highersame b),
                       lessthan = meet (lessthan a) (lessthan b),
                       equal = meet (equal a) (equal b),
                       greaterthan = meet (greaterthan a) (greaterthan b),
                       back = meet (back a) (back b) }


instance Lattice Value where
  bottom
    = Bottom
  join a b
    = case (a, b) of
           (CtrVal a, CtrVal b) -> CtrVal (join a b)
           (Bottom, a) -> a
           (a, Bottom) -> a
           (StdVal a, StdVal b) -> if True then StdVal b else error "std values do not match"

           (RegVal x, StdVal y) -> StdVal y
           (StdVal y, RegVal x) -> StdVal y

           (RegVal x, RegVal y) -> RegVal $ join x y
           (BackVal x, BackVal y) -> BackVal $ join x y
           (CharVal x, CharVal y) -> if x == y then CharVal y else error "char values do not match"

           (MemVal (x,a), MemVal (y,b)) -> MemVal (join x y, nub (a ++ b))
           (RegVal x, MemVal (y,b)) -> RegVal (join x y)
           (MemVal (x,a), RegVal y) -> RegVal (join x y)

           (RegVal x, ConVal y) -> ConVal $ join x y
           (ConVal x, RegVal y) -> ConVal $ join x y
           (ConVal x, ConVal y) -> ConVal $ join x y

  meet a b
    =  case (a, b) of
            (Bottom, Bottom) -> Bottom
            (Bottom, _) -> Bottom
            (_, Bottom) -> Bottom
            (RegVal x, RegVal y) -> if  disjoint x y then Bottom else RegVal (meet x y)
            (BackVal x, RegVal y) -> if  disjoint x y then Bottom else RegVal (meet x y)
            (RegVal x, BackVal y) -> if  disjoint x y then Bottom else RegVal (meet x y)
            (ConVal x, BackVal y) -> if  disjoint x y then Bottom else RegVal (meet x y)
            (ConVal x, RegVal y) -> if  disjoint x y then Bottom else RegVal (meet x y)

            (MemVal (x,a), RegVal y) -> if  disjoint x y then Bottom else MemVal (meet x y, a)
            (RegVal x, MemVal (y,a)) -> if  disjoint x y then Bottom else MemVal (meet x y, a)

            (MemVal (x,a), MemVal (y,b)) -> if  disjoint x y then Bottom else MemVal (meet x y, a++b)
            (StdVal a, StdVal b) -> StdVal b



-- | The container Registers indexed by the RegisterName
data Registers
  = Registers (Array.Array RegisterName Value) deriving (Eq)

-- | Order relation over the Register domain
instance Ord Registers where
  compare (Registers a) (Registers b)
    =  let a' = assocs a
           b' = assocs b
           f (aa, va) (ab,vb)
               = let x = case (va `le` vb) of
                              Just y -> y
                              Nothing -> error $ "compare registers " ++ show (aa,va,ab,vb)
                 in aa == ab && x
           --m = zipWith (\(aa,va) (ab,vb) -> aa == ab && fromJust (va `le` vb) ) a' b'
           m = zipWith f a' b'
       in if and m
             then LT
             else if a == b then EQ
             else GT


-- | Generic lattice closure for the Registers container
binaryRegisterOp (Registers a) (Registers b) f
  = let  (la, lb) = (Array.assocs a, Array.assocs b)
         l = zip la lb
         update a b = case cmp a b of { Nothing -> a;  _ -> f a b }
         m = \ ((ra, va), (rb, vb)) -> (assert (ra == rb) rb, update va vb)
         m' = map m l
    in (Registers (a Array.// m'))


-- | Wrapper Lattice instance for the container Registers
instance Lattice Registers where
   bottom
     =  Registers $ Array.listArray (R0, CPSR) (repeat Bottom)
   join a b
     = binaryRegisterOp a b join




-- | Lattice for the Stubs
instance Lattice Stubs where
  bottom
    = Map.empty
  join a b
    = Map.unionWith join a b
  meet a b
    = Map.intersectionWith meet a b




-- | Resolve name synonyms between RegisterNames
resolve FP =  R11
resolve IP =  R12
resolve SP =  R13
resolve LR =  R14
resolve PC =  R15
resolve r  =  r


-- | Get the value of the RegisterName inside the Registers container
getReg
  :: Registers
  -> RegisterName
  -> Value

getReg (Registers regs) regName
    = regs ! (resolve regName)


-- | Get the value of the RegisterName inside the Stubs container
getRegStub
  :: Stubs
  -> RegisterName
  -> Value

getRegStub stub regName
  = if  Map.member (resolve regName) stub
        then  stub Map.! (resolve regName)
        else  error $ "not found in stub's map " ++ show regName


-- | Set the Register value into the position RegisterName inside the Registers container
setReg
  :: Registers
  -> RegisterName
  -> Value
  -> Registers

setReg (Registers regs) regName regVal
  = Registers $ regs // [(resolve regName, regVal)]

setReg' (Registers regs) pair
  = Registers (regs // [pair])

-- | Set the Register value into the position RegisterName inside the Stubs container
setRegStub
  :: Stubs
  -> RegisterName
  -> Value
  -> Stubs

setRegStub stub regName regVal
  = Map.insert (resolve regName) regVal stub


-- | IO version of the CPSR flags
showCPSRFlags regs
  = do let lt =  cpsrGetLT regs
           eq =  cpsrGetEQ regs
           gt =  cpsrGetGT regs
           b  =  cpsrGetB regs
       putStr ("LT=" ++ show lt ++ " EQ=" ++ show eq ++ " GT=" ++ show gt ++ " B=" ++ show b)


-- | The LT flag is set to the bit 31
cpsrGetLT = cpsrGet 31
cpsrSetLT = cpsrSet 31
cpsrClearLT = cpsrClear 31

getControlLT = getControlBit 31
setControlLT = setControlBit 31
clearControlLT = clearControlBit 31

-- | The EQ flag is set to the bit 30
cpsrGetEQ = cpsrGet 30
cpsrSetEQ = cpsrSet 30
cpsrClearEQ = cpsrClear 30

getControlEQ = getControlBit 30
setControlEQ = setControlBit 30
clearControlEQ = clearControlBit 30

-- | The GT flag is set to the bit 29
cpsrGetGT = cpsrGet 29
cpsrSetGT = cpsrSet 29
cpsrClearGT = cpsrClear 29

getControlGT = getControlBit 29
setControlGT = setControlBit 29
clearControlGT = clearControlBit 29


-- | The B flag is set to the bit 28
cpsrGetB = cpsrGet 28
cpsrSetB = cpsrSet 28
cpsrClearB = cpsrClear 28

getControlB = getControlBit 28
setControlB = setControlBit 28
clearControlB = clearControlBit 28


-- | The C flag is set to the bit 27
cpsrGetC = cpsrGet 27
cpsrSetC = cpsrSet 27
cpsrClearC = cpsrClear 27

getControlC = getControlBit 27
setControlC = setControlBit 27
clearControlC = clearControlBit 27

-- | The I flag is set to the bit 26
cpsrGetI = cpsrGet 26
cpsrSetI = cpsrSet 26
cpsrClearI = cpsrClear 26

getControlI = getControlBit 26
setControlI = setControlBit 26
clearControlI = clearControlBit 26


-- | The LO flag is set to the bit 25
cpsrGetLO = cpsrGet 25
cpsrSetLO = cpsrSet 25
cpsrClearLO = cpsrClear 25

getControlLO = getControlBit 25
setControlLO = setControlBit 25
clearControlLO = clearControlBit 25


-- | The HI flag is set to the bit 24
cpsrGetHI = cpsrGet 24
cpsrSetHI = cpsrSet 24
cpsrClearHI = cpsrClear 24

getControlHI = getControlBit 24
setControlHI = setControlBit 24
clearControlHI = clearControlBit 24

-- | Get the control bit stored in the position Int from the CPSR register
cpsrGet
  :: Int
  -> Registers
  -> Word32

cpsrGet bit regs
  = do let  getBit = \c -> if c `testBit` bit then 1 else 0
       let  r  = getReg regs CPSR
       case r of
            CtrVal c -> getBit $ control c

-- | Get the control bit stored in the position Int directly from the CPSR word
getControlBit
  :: Int
  -> Word32
  -> Word32

getControlBit bit cpsr
  = if cpsr `testBit` bit then 1 else 0


-- | Set the control bit stored in the position Int into the container's CPSR register
cpsrSet
  :: Int
  -> Registers
  -> Registers

cpsrSet bit regs
  = do let r = getReg regs CPSR
       case r of
            CtrVal c ->  let cpsr = control c
                             c' = c {control = cpsr `setBit` bit}
                        in setReg regs CPSR (CtrVal c')

-- | Set the control bit stored in the position Int directly into the CPSR word
setControlBit
  :: Int
  -> Word32
  -> Word32

setControlBit bit cpsr
  = cpsr `setBit` bit


-- | Clear the control bit stored in the position Int from the container's CPSR register
cpsrClear
  :: Int
  -> Registers
  -> Registers

cpsrClear bit regs
  = do let r  = getReg regs CPSR
       case r of
            CtrVal c ->  let cpsr = control c
                             c' = c { control = cpsr `clearBit` bit }
                        in setReg regs CPSR (CtrVal c')

-- | Clear the control bit stored in the position Int directly from the CPSR word
clearControlBit
  :: Int
  -> Word32
  -> Word32

clearControlBit bit cpsr
  = cpsr `clearBit` bit


-- | Get the the interval above the interval provided
above
  :: Value
  -> Value

above (RegVal (l,_))
  = RegVal (l, fromIntegral max_value :: Word32)
above Bottom
  = Bottom

-- | Get the the interval below the interval provided
below
  :: Value
  -> Value

below (RegVal (_,u))
  = RegVal (fromIntegral min_value :: Word32, u)
below Bottom
  = Bottom


-- | Get the state of the control bits associated with to each RegisterName
cpsrGetByName R0 = getControlBit 27
cpsrGetByName R1 = getControlBit 26
cpsrGetByName R2 = getControlBit 25
cpsrGetByName R3 = getControlBit 24
cpsrGetByName R4 = getControlBit 23
cpsrGetByName R5 = getControlBit 22
cpsrGetByName R6 = getControlBit 21
cpsrGetByName R7 = getControlBit 20
cpsrGetByName R8 = getControlBit 19
cpsrGetByName R9 = getControlBit 18
cpsrGetByName R10 = getControlBit 17
cpsrGetByName R11 = getControlBit 16
cpsrGetByName R12 = getControlBit 15
cpsrGetByName R13 = getControlBit 14
cpsrGetByName R14 = getControlBit 13
cpsrGetByName R15 = getControlBit 12
cpsrGetByName CPSR = getControlBit 11

-- | Clear the state of the control bits associated with to each RegisterName
cpsrClearByName R0 = clearControlBit 27
cpsrClearByName R1 = clearControlBit 26
cpsrClearByName R2 = clearControlBit 25
cpsrClearByName R3 = clearControlBit 24
cpsrClearByName R4 = clearControlBit 23
cpsrClearByName R5 = clearControlBit 22
cpsrClearByName R6 = clearControlBit 21
cpsrClearByName R7 = clearControlBit 20
cpsrClearByName R8 = clearControlBit 19
cpsrClearByName R9 = clearControlBit 18
cpsrClearByName R10 = clearControlBit 17
cpsrClearByName R11 = clearControlBit 16
cpsrClearByName R12 = clearControlBit 15
cpsrClearByName R13 = clearControlBit 14
cpsrClearByName R14 = clearControlBit 13
cpsrClearByName R15 = clearControlBit 12
cpsrClearByName CPSR = clearControlBit 11

-- | Set the state of the control bits associated with to each RegisterName
cpsrSetByName R0 = setControlBit 27
cpsrSetByName R1 = setControlBit 26
cpsrSetByName R2 = setControlBit 25
cpsrSetByName R3 = setControlBit 24
cpsrSetByName R4 = setControlBit 23
cpsrSetByName R5 = setControlBit 22
cpsrSetByName R6 = setControlBit 21
cpsrSetByName R7 = setControlBit 20
cpsrSetByName R8 = setControlBit 19
cpsrSetByName R9 = setControlBit 18
cpsrSetByName R10 = setControlBit 17
cpsrSetByName R11 = setControlBit 16
cpsrSetByName R12 = setControlBit 15
cpsrSetByName R13 = setControlBit 14
cpsrSetByName R14 = setControlBit 13
cpsrSetByName R15 = setControlBit 12
cpsrSetByName CPSR = setControlBit 11

-- | Show the contents of the RegisterName flags inside the control word
showRegisterFlags :: Word32 -> String
showRegisterFlags cpsr
  = let r0 = cpsrGetByName R0 cpsr
        r1 = cpsrGetByName R1 cpsr
        r2 = cpsrGetByName R2 cpsr
        r3 = cpsrGetByName R3 cpsr
        r4 = cpsrGetByName R4 cpsr
        r5 = cpsrGetByName R5 cpsr
        r6 = cpsrGetByName R6 cpsr
        r7 = cpsrGetByName R7 cpsr
        r8 = cpsrGetByName R8 cpsr
        r9 = cpsrGetByName R9 cpsr
        r10 = cpsrGetByName R10 cpsr
        r11 = cpsrGetByName R11 cpsr
        r12 = cpsrGetByName R12 cpsr
        r13 = cpsrGetByName R13 cpsr
        r14 = cpsrGetByName R14 cpsr
        r15 = cpsrGetByName R15 cpsr
        c = cpsrGetByName CPSR cpsr
        p = r0:r1:r2:r3:r4:r5:r6:r7:r8:r9:r10:r11:r12:r13:r14:r15:c:[]
        p' = [ x | x <- p, x == 1 ]
    in show p

