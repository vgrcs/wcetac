----------------------------------------------------------------------
-- FILE:              Encoder.hs
-- DATE:              03/04/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Encoder
  ( encode )
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Int
import Data.Word
import Data.Array



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Operand
import Arm.RegisterName
import Arm.Decoder



----------------------------------------------------------------------
-- Encoding shortcuts.
----------------------------------------------------------------------
condEq :: (Int, Int, Word64)
condNe :: (Int, Int, Word64)
condCs :: (Int, Int, Word64)
condHs :: (Int, Int, Word64)
condCc :: (Int, Int, Word64)
condLo :: (Int, Int, Word64)
condMi :: (Int, Int, Word64)
condPl :: (Int, Int, Word64)
--condVs :: (Int, Int, Word64)
condVc :: (Int, Int, Word64)
condHi :: (Int, Int, Word64)
condLs :: (Int, Int, Word64)
condGe :: (Int, Int, Word64)
condLt :: (Int, Int, Word64)
condGt :: (Int, Int, Word64)
condLe :: (Int, Int, Word64)
condAl :: (Int, Int, Word64)
condNv :: (Int, Int, Word64)

condEq = (31, 28, 0x6)
condNe = (31, 28, 0x1)
condCs = (31, 28, 0x2)
condHs = (31, 28, 0x2)
condCc = (31, 28, 0x3)
condLo = (31, 28, 0x3)
condMi = (31, 28, 0x4)
condPl = (31, 28, 0x5)
--condVs = (31, 28, 0x6)
condVc = (31, 28, 0x7)
condHi = (31, 28, 0x8)
condLs = (31, 28, 0x9)
condGe = (31, 28, 0xA)
condLt = (31, 28, 0xB)
condGt = (31, 28, 0xC)
condLe = (31, 28, 0xD)
condAl = (31, 28, 0x0)
condNv = (31, 28, 0xF)


----------------------------------------------------------------------
-- Split a word into fields.
----------------------------------------------------------------------
splitWord_
  :: Word64
  -> (Int, Int)
  -> Word64

splitWord_ word (hi, lo)
  = let mask = (2 ^ (hi - lo + 1) - 1) `shiftL` lo
    in (word .&. mask) `shiftR` lo


----------------------------------------------------------------------
-- Encode an instruction into a Word32.
----------------------------------------------------------------------
encode
  :: Instruction
  -> Word64

encode (Printf (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                  -- type
             , (22, 14, 0x01)                 -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)    -- constant
             ]
    in w

encode (PThreadCreate (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                  -- type
             , (22, 14, 0x02)                 -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)    -- constant
             ]
    in w

encode (PThreadExit (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                  -- type
             , (22, 14, 0x03)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Exit (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                  -- type
             , (22, 14, 0x04)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w


encode (PthreadMutexLock (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x05)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadCondWait (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x06)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexUnlock (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x07)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexDestroy (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x08)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadCondSignal (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x09)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexattrInit (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x0A)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexattrSetpshared (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x0B)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexInit (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x0C)                  -- opcode
             , (27, 25, 0x6)                  -- opcode
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (PthreadMutexattrDestroy (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x1)                 -- type
             , (22, 14, 0x0D)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w


encode (ShmOpen (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x2)                 -- type
             , (22, 14, 0x01)                 -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w


encode (ShmUnlink (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x2)                 -- type
             , (22, 14, 0x02)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Ftruncate (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x2)                 -- type
             , (22, 14, 0x03)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Mmap (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x2)                 -- type
             , (22, 14, 0x04)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Munmap (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x2)                 -- type
             , (22, 14, 0x5)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Fork (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x3)                 -- type
             , (22, 14, 0x01)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Waitpid (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x3)                 -- type
             , (22, 14, 0x02)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w

encode (Generic (Con cycles))
  = let w =  concatFields 0
             [ (13, 9, 0x3)                 -- type
             , (22, 14, 0x03)                  -- opcode
             , (27, 25, 0x6)                  -- switch decoder
             , (8, 0, fromIntegral cycles)   -- constant
             ]
    in w


----------------------------------------
-- add three registers
encode (Add (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x04)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(25, 25, 0), (3, 0, regIndex r3)]   -- second operand is register
                  Con c1
                   -- -> [(25, 25, 1), (7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(25, 25, 1), (11, 0, to16to32to64 c1)]            -- 16-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- logical bit-wise and
encode (And (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x08)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                   -- -> [(7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [ (11, 0, to16to32to64 c1)
                       ,(25, 25, 0x01)
                      ]            -- 16-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- branch unconditionally
encode (B (Rel rel))
  = encodeBranch  (31, 28, 0xE) rel


----------------------------------------
-- branch if equal
encode (Beq (Rel rel))
  = encodeBranch condEq rel

----------------------------------------
-- branch if greater than
encode (Bgt (Rel rel))
  = encodeBranch condGt rel

----------------------------------------
-- bit clear
encode (Bic (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x0E)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                   -- -> [(7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(11, 0, to16to32to64 c1)]            -- 16-bit constant
               )
    in w1 .|. w2


----------------------------------------
-- load a label into a register
encode (Adr (Reg r1)  (Rel label))
  = if label > 0
       then  concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x06)         -- opcode
               , (19, 16, regIndex r1)  -- first operand register
               , (25, 25, 0x00)
               , (7, 0, fromIntegral label)
               , (11, 11, 0x00)
               , (28, 28, 0x00)
               ]
       else concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x06)         -- opcode
               , (19, 16, regIndex r1)  -- first operand register
               , (25, 25, 0x00)
               , (11, 11, 0x00)
               , (7, 0, ((fromIntegral . abs) label))
               , (28, 28, 0x01)
               ]



----------------------------------------
-- branch and link
encode (Bl (Rel rel))
  = encodeBranch (31, 28, 0xE) rel .|. concatFields 0 [(24, 24, 1)]

----------------------------------------
-- branch if less than
encode (Blt (Rel rel))
  = encodeBranch condLt rel

----------------------------------------
-- branch if not equal
encode (Bne (Rel rel))
  = encodeBranch condNe rel

----------------------------------------
-- branch if greater than or equal
encode (Bge (Rel rel))
  = encodeBranch condGe rel

----------------------------------------
-- branch if less than or equal
encode (Ble (Rel rel))
  = encodeBranch condLe rel


----------------------------------------
-- branch if below or equal
encode (Bls (Rel rel))
  = encodeBranch condLs rel


----------------------------------------
-- compare two operands
encode (Cmp (Reg r1) op2)
  = let w1 = concatFields 0
               [ condAl                  -- condition
               , (24, 21, 0x0A)          -- opcode
               , (15, 12, regIndex r1)   -- register 1
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

----------------------------------------
-- logical bit-wise exclusive or
encode (Eor (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x01)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                   -- -> [(7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(11, 0, to16to32to64 c1)]            -- 16-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- load multiple registers
encode (Ldmia op1 (Mrg regs))
  = encodeMReg 0x0 0x0 op1 regs

encode (Ldmfd op1 (Mrg regs))
  = encodeMReg 0x0 0x0 op1 regs

----------------------------------------
-- load register
encode (Ldr (Reg r1) op2)
  = encodeLdrStr 0x0 0x1 0x0 r1 op2  condAl

----------------------------------------
-- load register, unsigned byte
encode (Ldrb (Reg r1) op2)
  = encodeLdrStr 0x0 0x0 0x0 r1 op2  condAl

-- load register, half word
encode (Ldrh (Reg r1) op2)
  = encodeLdrStr 0x0 0x0 0x1 r1 op2  condAl

-- load register, half word
encode (Ldrsh (Reg r1) op2)
  = encodeLdrStr 0x0 0x1 0x1 r1 op2  condAl

----------------------------------------
-- move register to register
encode (Mov (Reg r1) op2)
  = let w1 = concatFields 0
               [ condAl                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Movlt (Reg r1) op2)
  = let w1 = concatFields 0
               [ condLt                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Movne (Reg r1) op2)
  = let w1 = concatFields 0
               [ condNe                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Moveq (Reg r1) op2)
  = let w1 = concatFields 0
               [ condEq                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Movls (Reg r1) op2)
  = let w1 = concatFields 0
               [ condLs                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Movhi (Reg r1) op2)
  = let w1 = concatFields 0
               [ condHi                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

encode (Movge (Reg r1) op2)
  = let w1 = concatFields 0
               [ condGe                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

----------------------------------------
-- move register to register
encode (Mvn (Reg r1) op2)
  = let w1 = concatFields 0
               [ condAl                  -- condition
               , (24, 21, 0x05)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

----------------------------------------
-- multiply
encode (Mul (Reg r1) (Reg r2) (Reg r3))
  = concatFields 0
      [ condAl
      , (19, 16, regIndex r1)
      , (11,  8, regIndex r3)
      , ( 7,  7, 0x1)
      , ( 4,  4, 0x1)
      , ( 3,  0, regIndex r2)
      ]

----------------------------------------
-- multiply
encode (SMul (Reg r1) (Reg r2) (Reg r3) (Reg r4))
  = concatFields 0
      [ condAl
      , (19, 16, regIndex r1)
      , (11,  8, regIndex r3)
      , ( 7,  7, 0x1)
      , ( 4,  4, 0x0)
      , ( 3,  0, regIndex r2)
      , ( 15,  12, regIndex r4)
      ]

----------------------------------------
-- logical bit-wise or
encode (Orr (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x0C)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                   -- -> [(7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(11, 0, to16to32to64 c1)]            -- 16-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- load multiple registers
encode (Stmia op1 (Mrg regs))
  = encodeMReg 0x1 0x1 op1 regs

encode (Stmfd op1 (Mrg regs))
  = encodeMReg 0x1 0x0 op1 regs
----------------------------------------
-- store register
encode (Str (Reg r1) op2)
  = encodeLdrStr 0x1 0x1 0x0 r1 op2  condAl

----------------------------------------
-- store register
encode (Strh (Reg r1) op2)
  = encodeLdrStr 0x1 0x0 0x1 r1 op2  condAl

----------------------------------------
-- store register, unsigned byte
encode (Strb (Reg r1) op2)
  = encodeLdrStr 0x1 0x0 0x0 r1 op2   condAl

----------------------------------------
-- add three registers
encode (Sub (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x02)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -- -> [(25, 25, 1), (7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(25, 25, 1), (11, 0, to16to32to64 c1)]            -- 16-bit constant (11)

               )
    in  w1 .|. w2


----------------------------------------
-- add three registers
encode (Rsb (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x03)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -- -> [(25, 25, 1), (7, 0, to8to16to32 c1)]            -- 8-bit constant
                    -> [(25, 25, 1), (11, 0, to16to32to64 c1)]            -- 16-bit constant (11)

               )
    in  w1 .|. w2

----------------------------------------
-- software interrupt
encode (Swi (Con c))
  = concatFields 0 [ condAl
                   , (27, 24, 0xF)
                   ] .|. (to16to32to64 c)


encode (b) = error ("Encode: " ++ show b)
----------------------------------------------------------------------
-- helper functions

encodeBranch cond rel
  = concatFields 0 [ cond,
  (27, 25, 0x5)
  ] .|. (to16to32to64 rel)


to16to32 n = (fromIntegral (fromIntegral n :: Word16) :: Word32)

to16to32to64 n = (fromIntegral (fromIntegral (fromIntegral n :: Word16) :: Word32) :: Word64)

to8to16to32 n = (fromIntegral (fromIntegral (fromIntegral n :: Word8) :: Word16) :: Word32)

encodeOp2 op
  = concatFields 0
      (case op of
        Reg r2
          -> [(3, 0, regIndex r2)]    -- first operand register
        Con c1
          -> [ (25, 25, 0x01)         -- ``#'' field
             --, (7, 0, to8to16to32 c1)             -- 8-bit immediate
             , (11, 0, to16to32to64 c1)             -- 16-bit immediate
             ]
        ArithShiftL r c1
          -> [  (3, 0, regIndex r)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)             -- 8-bit immediate
             , (33, 33, 0x00)             -- 8-bit immediate
             , (34, 34, 0x00)
             , (35, 35, 0x00)
             , (9, 4, to16to32to64 c1)             -- 16-bit immediate
             ]
        ArithShiftR r c1
          -> [  (3, 0, regIndex r)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)             -- 8-bit immediate
             , (33, 33, 0x01)             -- 8-bit immediate
             , (34, 34, 0x00)
             , (35, 35, 0x00)
             , (9, 4, to16to32to64 c1)             -- 16-bit immediate
             ]
        BinShiftL r c1
          -> [  (3, 0, regIndex r)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)         -- ``#'' field
             , (33, 33, 0x00)             -- 8-bit immediate
             , (34, 34, 0x01)
             , (35, 35, 0x00)
             , (9, 4, to16to32to64 c1)             -- 16-bit immediate
             ]
        BinShiftR r c1
          -> [  (3, 0, regIndex r)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)         -- ``#'' field
             , (33, 33, 0x01)             -- 8-bit immediate
             , (34, 34, 0x01)             -- 8-bit immediate
             , (35, 35, 0x00)
             , (9, 4, to16to32to64 c1)             -- 16-bit immediate
             ]
        ArithRegShiftL r1 r2
          -> [  (3, 0, regIndex r1)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)             -- 8-bit immediate
             , (33, 33, 0x00)             -- 8-bit immediate
             , (34, 34, 0x00)
             , (35, 35, 0x01)
             , (7, 4, regIndex r2)             -- 16-bit immediate
             ]
        ArithRegShiftR r1 r2
          -> [  (3, 0, regIndex r1)
             , (25, 25, 0x00)         -- ``#'' field
             , (32, 32, 0x01)             -- 8-bit immediate
             , (33, 33, 0x01)             -- 8-bit immediate
             , (34, 34, 0x00)
             , (35, 35, 0x01)
             , (7, 4, regIndex r2)             -- 16-bit immediate
             ]
       )

-- encode a multiple register load or store
encodeMReg ls mode op1 regs
  = let w1 = concatFields 0
               [ condAl
               , (27, 25, 0x04)    -- opcode
             --, (24, 24, 0x00)    -- post-increment or decrement
               , (23, 23, 0x01)    -- increment or decrement
               , (20, 20, ls)      -- load
               , (39, 39, mode)    -- fd,ia
               ]
        w2 = concatFields 0
               (case op1 of
                  Aut (Reg reg)
                    -> [ (21, 21, 0x01)   -- write-back
                       , (19, 16, regIndex reg)
                       ]
                  Reg reg
                    -> [ (19, 16, regIndex reg)
                       ]
               )
        w3 = concatFields 0
               (map (\reg -> let i = fromIntegral (regIndex reg) in (i, i, 1)) regs)
    in w1 .|. w2 .|. w3

-- encode a load or store
encodeLdrStr ls bw extra r1 op2 cond
  = let w1 = concatFields 0
               [ condAl                   -- condition
               , (27, 26, 0x01)         -- constant field
             --, (25, 25, 0x00)         -- ``#'' field
             --, (23, 23, 0x00)         -- up/down
               , (25, 25, extra)         -- extra from strh
               , (22, 22, bw)           -- unsigned byte/word
               , (20, 20, ls)           -- load/store
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  BasShift r1 sr
                    -> case sr of
                           ArithShiftL r  off
                             -> [    (19, 16, regIndex r1) -- base register
                                  , (11, 8,  regIndex r) -- shift register
                                  , (7,  0, fromIntegral off)      -- offset
                                  , (37, 37, 0x00)
                                  , (23, 23, 0x01)        -- shift
                               ]
                           ArithShiftR r  off
                             -> [    (19, 16, regIndex r1) -- base register
                                  , (11, 8,  regIndex r) -- shift register
                                  , (7,  0, fromIntegral off)      -- offset
                                  , (37, 37, 0x01)
                                  , (23, 23, 0x01)        -- shift
                               ]
                           BinShiftL r  off
                             -> [    (19, 16, regIndex r1) -- base register
                                  , (11, 8,  regIndex r) -- shift register
                                  , (7,  0, fromIntegral off)      -- offset
                                  , (37, 37, 0x00)
                                  , (23, 23, 0x01)        -- shift
                                  , (21, 21, 0x01)        -- shift
                                  , (24, 24, 0x01)        -- shift
                               ]
                           BinShiftR r  off
                             -> [    (19, 16, regIndex r1) -- base register
                                  , (11, 8,  regIndex r) -- shift register
                                  , (7,  0, fromIntegral off)      -- offset
                                  , (37, 37, 0x01)
                                  , (23, 23, 0x01)        -- shift
                                  , (21, 21, 0x01)        -- shift
                                  , (24, 24, 0x01)        -- shift
                               ]
                           Reg r
                             -> [    (19, 16, regIndex r1) -- base register
                                  , (11, 8,  regIndex r) -- shift register
                                  , (23, 23, 0x01)        -- shift
                                  , (21, 21, 0x00)        -- shift
                                  , (24, 24, 0x01)        -- shift
                               ]
                           other -> error $ "encodeLdrStr " ++ (show other)
                  Ind r2
                    -> [ --(24, 24, 0x00)        -- pre/post index
                     --, (21, 21, 0x00)        -- write-back (auto-index)
                         (19, 16, regIndex r2) -- base register
                       ]
                  Bas r2 offset
                    -> if offset > 0
                          then [ (19, 16, regIndex r2) -- base register
                               , (11,  0,  (fromIntegral offset))      -- offset
                               , (36, 36, 0x00)
                               ]
                          else [ (19, 16, regIndex r2) -- base register
                               , (11,  0,  ((fromIntegral . abs) offset))      -- offset
                               , (36, 36, 0x01)
                               ]
                  Aut (Bas r2 offset)
                    -> if offset > 0
                          then [ (21, 21, 0x01)        -- write-back (auto-index)
                               , (19, 16, regIndex r2) -- base register
                               , (11,  0, (fromIntegral offset))      -- offset
                               , (36, 36, 0x00)
                               ]
                          else [ (21, 21, 0x01)        -- write-back (auto-index)
                               , (19, 16, regIndex r2) -- base register
                               , (11,  0, ((fromIntegral . abs) offset))      -- offset
                               , (36, 36, 0x01)
                               ]
                  Pos (Ind r2) const
                    -> if const > 0
                          then [ (24, 24, 0x01)        -- pre/post index
                               , (21, 21, 0x01)        -- write-back (auto-index)
                               , (19, 16, regIndex r2) -- base register
                               , (11,  0, (fromIntegral const))       -- offset
                               , (36, 36, 0x0)
                               ]
                          else [ (24, 24, 0x01)        -- pre/post index
                               , (21, 21, 0x01)        -- write-back (auto-index)
                               , (19, 16, regIndex r2) -- base register
                               , (11,  0, ((fromIntegral . abs) const))       -- offset
                               , (36, 36, 0x01)
                               ]
                  Con offset
                    ->  [ (24, 24, 0x01)
                          , (11, 0, fromIntegral offset) -- base register
                       ]
                  other -> error $ "encode" ++ show other
               )
	res = w1 .|. w2
    in  w1 .|. w2
    {-in case op2 of
            Con _ -> error (show (op2, splitWord w2 (11, 0)))
            _ -> w1 .|. w2-}




----------------------------------------------------------------------
-- Concatenate bit fields into one word.
----------------------------------------------------------------------
concatFields
  :: Word64
  -> [(Int, Int, Word64)]
  -> Word64

concatFields word [] = word
concatFields word ((hi, lo, val) : fields)
  = let mask = fromIntegral (2 ^ (hi - lo + 1) - 1)
        val' = val .&. mask
    in concatFields (word .|. (val' `shiftL` lo)) fields



----------------------------------------------------------------------
-- Convert a register name into a word32.
----------------------------------------------------------------------
regIndex
  :: RegisterName
  -> Word64

regIndex FP = (fromIntegral . (index (R0, CPSR))) R11
regIndex IP = (fromIntegral . (index (R0, CPSR))) R12
regIndex SP = (fromIntegral . (index (R0, CPSR))) R13
regIndex LR = (fromIntegral . (index (R0, CPSR))) R14
regIndex PC = (fromIntegral . (index (R0, CPSR))) R15
regIndex r  = (fromIntegral . (index (R0, CPSR))) r



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
