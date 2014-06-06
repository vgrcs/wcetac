----------------------------------------------------------------------
-- FILE:              RegisterName.hs
-- DATE:              2/6/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.RegisterName
  ( RegisterName(..)
  , nthReg
  )
where


import Data.Word
import Data.Array


----------------------------------------------------------------------
-- Data type for register names.
----------------------------------------------------------------------
data RegisterName
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | CPSR
  | FP
  | IP
  | SP
  | LR
  | PC
  | SL
  deriving (Enum, Ix, Ord, Read) --Eq,

instance Show RegisterName where
  show r = show' r


-- |
instance Eq RegisterName where
  FP == R11 = True
  IP == R12 = True
  SP == R13 = True
  LR == R14 = True
  PC == R15 = True
  FP == FP = True
  IP == IP = True
  SP == SP = True
  LR == LR = True
  PC == PC = True
  R11 == FP = True
  R12 == IP = True
  R13 == SP = True
  R14 == LR = True
  R15 == PC = True
  R0 == R0  = True
  R1 == R1  = True
  R2 == R2  = True
  R3 == R3  = True
  R4 == R4  = True
  R5 == R5  = True
  R6 == R6  = True
  R7 == R7  = True
  R8 == R8  = True
  R9 == R9  = True
  R10 == R10 = True
  R11 == R11 = True
  R12 == R12 = True
  R13 == R13 = True
  R14 == R14 = True
  R15 == R15 = True
  CPSR == CPSR = True
  a == b   =  False

----------------------------------------------------------------------
----------------------------------------------------------------------
nthReg :: Word64 -> RegisterName
nthReg  0 = R0
nthReg  1 = R1
nthReg  2 = R2
nthReg  3 = R3
nthReg  4 = R4
nthReg  5 = R5
nthReg  6 = R6
nthReg  7 = R7
nthReg  8 = R8
nthReg  9 = R9
nthReg 10 = R10
nthReg 11 = R11
nthReg 12 = R12
nthReg 13 = R13
nthReg 14 = R14
nthReg 15 = R15
nthReg _  = error ("nthReg not found")


----------------------------------------------------------------------
-- Convert a register name to a string.
----------------------------------------------------------------------
show' R0   = "r0"
show' R1   = "r1"
show' R2   = "r2"
show' R3   = "r3"
show' R4   = "r4"
show' R5   = "r5"
show' R6   = "r6"
show' R7   = "r7"
show' R8   = "r8"
show' R9   = "r9"
show' R10  = "r10"
show' R11  = "fp "  -- "r11"
show' R12  = "ip"  -- "r12"
show' R13  = "sp"  -- "r13"
show' R14  = "lr"  -- "r14"
show' R15  = "pc"  -- "r15"
show' CPSR = "cpsr"
show' SP   = "sp"
show' FP   = "fp"
show' IP   = "ip"
show' LR   = "lr"
show' PC   = "pc"
show' SL   = "sl"

----------------------------------------------------------------------
-- Convert a string to a register name.
----------------------------------------------------------------------
read' "r0"   = R0
read' "r1"   = R1
read' "r2"   = R2
read' "r3"   = R3
read' "r4"   = R4
read' "r5"   = R5
read' "r6"   = R6
read' "r7"   = R7
read' "r8"   = R8
read' "r9"   = R9
read' "r10"  = R10
read' "r11"  = R11
read' "r12"  = R12
read' "r13"  = R13
read' "r14"  = R14
read' "r15"  = R15
read' "cpsr" = CPSR
read' "sp"   = SP
read' "fp"   = FP
read' "ip"   = IP
read' "lr"   = LR
read' "pc"   = PC

----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
