----------------------------------------------------------------------
-- FILE:              Decoder.hs
-- DATE:              03/05/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Decoder
  ( decode, splitWord )
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Int
import Data.Maybe
import Debug.Trace
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Operand
import Arm.RegisterName



----------------------------------------------------------------------
-- Decode a word into an instruction.
----------------------------------------------------------------------
decode
  :: Word64
  -> Maybe Instruction

decode word
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        destReg = nthReg (bits (15, 12))
        firstOp = nthReg (bits (19, 16))
        op2 = case (bit 35, bit 34, bit 33, bit 32, bit 25) of
                (0,0,0,0,0)  -- register
                  -> Reg (nthReg (bits (3, 0)))
                (0,0,0,0,1)  -- (16 truncated to 11) 8-bit immediate
                  -> let c = fromIntegral (bits (11, 0))
                    in Con c
                (0,0,0,1,0)
                  -> let r = nthReg (bits (3, 0))
                         off = bits (9, 4)
                    in ArithShiftL r (fromIntegral off)
                (0,0,1,1,0)
                  -> let r = nthReg (bits (3, 0))
                         off = bits (9, 4)
                    in ArithShiftR r (fromIntegral off)
                (0,1,0,1,0)
                  -> let r = nthReg (bits (3, 0))
                         off = bits (9, 4)
                    in BinShiftL r (fromIntegral off)
                (0,1,1,1,0)
                  -> let r = nthReg (bits (3, 0))
                         off = bits (9, 4)
                    in BinShiftR r (fromIntegral off)
                (1,0,0,1,0)
                  -> let r1 = nthReg (bits (3, 0))
                         r2 = nthReg (bits (7, 4))
                    in ArithRegShiftL r1 r2
                (1,0,1,1,0)
                  -> let r1 = nthReg (bits (3, 0))
                         r2 = nthReg (bits (7, 4))
                    in ArithRegShiftR r1 r2

    in case bits (27, 24) of
         0xF -> let s = fromIntegral (bits (23, 0))
               --in Just (Swi (Con (convertWordToInt s)))
               in Just (Swi (Con s))
         _   -> case bits (27, 25) of
                  0x4 -> decodeMReg word firstOp -- multiple register transfer
                  0x5 -> decodeBranch word
                  0x6 -> decodeConst word
                  _   -> case (bits (27, 26)) of
                           0x0  -- multiplication or data processing instructions
                             -> decodeMulOrDp word destReg firstOp op2
                           0x1  -- data transfer instructions
                             -> decodeDataTrans word destReg
                           _ -> Nothing


----------------------------------------
decodeConst word
  = let bits = splitWord word
    in case bits (13, 9) of
            0x1 -> case bits (22, 14) of
                        0x01 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Printf (Con cycles))
                        0x02 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PThreadCreate (Con cycles))
                        0x03 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PThreadExit (Con cycles))
                        0x04 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Exit (Con cycles))
                        0x05 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexLock (Con cycles))
                        0x06 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadCondWait (Con cycles))
                        0x07 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexUnlock (Con cycles))
                        0x08 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexDestroy (Con cycles))
                        0x09 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadCondSignal (Con cycles))
                        0x0A -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexattrInit (Con cycles))
                        0x0B -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexattrSetpshared (Con cycles))
                        0x0C -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexInit (Con cycles))
                        0x0D -> let cycles = fromIntegral (bits (8, 0))
                              in Just (PthreadMutexattrDestroy (Con cycles))
            0x2 -> case bits (22, 14) of
                        0x01 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (ShmOpen (Con cycles))
                        0x02 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (ShmUnlink (Con cycles))
                        0x03 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Ftruncate (Con cycles))
                        0x04 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Mmap (Con cycles))
                        0x05 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Munmap (Con cycles))
            0x3 -> case bits (22, 14) of
                        0x01 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Fork (Con cycles))
                        0x02 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Waitpid (Con cycles))
                        0x03 -> let cycles = fromIntegral (bits (8, 0))
                              in Just (Generic (Con cycles))

----------------------------------------
decodeMulOrDp word destReg firstOp op2
  = let bits = splitWord word
        bit x = splitWord word (x, x)
    in case (bits (27, 24), bit 7, bit 4) of
         (0, 1, 1)
           -> let rm = nthReg (bits (3, 0)) --snd
                  rd = nthReg (bits (19, 16)) --fst
                  rs = nthReg (bits (11, 8)) --
              in Just (Mul (Reg rd) (Reg rm) (Reg rs))
         (0, 1, 0)
           -> let rm = nthReg (bits (3, 0)) --snd
                  rd = nthReg (bits (19, 16)) --fst
                  rs = nthReg (bits (11, 8))
              in Just (SMul (Reg rd) (Reg rm) (Reg rs) (Reg destReg))
         _ -> decodeDataProc (bits (24, 21)) destReg firstOp op2 word


convertWordToInt :: Word32 -> Int32
convertWordToInt w
  = let w' = fromIntegral w
    in if w' > 32767
          then w' - 65536
          else w'
             -- if  offset > 8388607 -- this is 2^23 - 1
             --        then offset - 16777216 -- this is  2^24
             --        else offset

----------------------------------------
decodeBranch word
  = let link = splitWord word (24, 24)
        offset = fromIntegral (splitWord word (23, 0))
        offset' = if offset > 32767
                    then offset - 65536
                    else offset
             -- if  offset > 8388607 -- this is 2^23 - 1
             --        then offset - 16777216 -- this is  2^24
             --        else offset




            -- if offset > 32767
            --         then offset - 65536
            --         else offset



        cond = splitWord word (31, 28)
    in case link of
         0x0
           -> case cond of
                0x6 -> Just (Beq (Rel offset'))
                0x1 -> Just (Bne (Rel offset'))
                0x9 -> Just (Bls (Rel offset'))
                0xA -> Just (Bge (Rel offset'))
                0xB -> Just (Blt (Rel offset'))
                0xC -> Just (Bgt (Rel offset'))
                0xD -> Just (Ble (Rel offset'))
                0xE -> Just (B (Rel offset'))
                _   -> Nothing
         0x1
           -> case cond of
                0xE -> Just (Bl (Rel offset'))
                _   -> Nothing

----------------------------------------
decodeDataProc opcode destReg firstOp op2 word
  = let bits = splitWord word
        bit x = splitWord word (x, x)
    in case opcode of
       0x08 -> Just (And (Reg destReg) (Reg firstOp) op2)
       0x01 -> Just (Eor (Reg destReg) (Reg firstOp) op2)
       0x02 -> Just (Sub (Reg destReg) (Reg firstOp) op2)
       0x03 -> Just (Rsb (Reg destReg) (Reg firstOp) op2)
       0x04 -> Just (Add (Reg destReg) (Reg firstOp) op2)
       0x0A -> Just (Cmp (Reg destReg) op2)
       0x0C -> Just (Orr (Reg destReg) (Reg firstOp) op2)
       0x0D -> case (bits (31,28)) of
                    (0x0) -> Just (Mov (Reg destReg) op2)
                    (0xB) -> Just (Movlt (Reg destReg) op2)
                    (0x1) -> Just (Movne (Reg destReg) op2)
                    (0x6) -> Just (Moveq (Reg destReg) op2)
                    (0x9) -> Just (Movls (Reg destReg) op2)
                    (0x8) -> Just (Movhi (Reg destReg) op2)
                    (0xA) -> Just (Movge (Reg destReg) op2)
                    other -> error $ "decode " ++ show other
       0x05 -> Just (Mvn (Reg destReg) op2)
       0x06 -> Just (Adr (Reg destReg) op2)
       0x0E -> Just (Bic (Reg destReg) (Reg firstOp) op2)
       _    -> Nothing

----------------------------------------
decodeMReg word firstOp
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        instr = case (bit 39, bit 20) of
                     (0x0, 0x0) -> Ldmfd
                     (0x0, 0x1) -> Stmfd
                     (0x1, 0x1) -> Stmia
        rn = case (bit 21) of
               0x0 -> Reg firstOp
               0x1 -> (Aut (Reg firstOp))
        regList 0 _ = []
        regList n regNum
          | odd n  = (nthReg regNum) : (regList (n `div` 2) (regNum + 1))
          | even n = regList (n `div` 2) (regNum + 1)
        regs = regList (fromIntegral (bits (15, 0))) 0
    in Just (instr rn (Mrg regs))


----------------------------------------
decodeDataTrans word destReg
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        instr = case (bit 25, bit 22, bit 20) of
                  (0, 0, 0) -> Ldrb
                  (0, 0, 1) -> Strb
                  (0, 1, 0) -> Ldr
                  (0, 1, 1) -> Str
                  (1, 0, 0) -> Ldrh
                  (1, 0, 1) -> Strh
                  (1, 1, 0) -> Ldrsh
                  (1, 1, 1) -> Strsh
        baseReg = nthReg (bits (19, 16))
        offset = bits (11, 0)
        neg = bit 36
        addrMode = (bit 23, bit 21, bit 24)
        op2 = case addrMode of
                (0,0,0) -> if offset == 0
                             then Just (Ind baseReg)
                             else if neg == 0x00
                                     then Just (Bas baseReg (fromIntegral offset))
                                     else Just (Bas baseReg (-(fromIntegral offset)))
                (0,0,1) -> let addr = (bits (11, 0))
                          in Just (Con (fromIntegral addr))
                (0,1,0) -> if neg == 0x00
                             then Just (Aut (Bas baseReg (fromIntegral offset)))
                             else Just (Aut (Bas baseReg (-(fromIntegral offset))))
                (0,1,1) -> if neg == 0x00
                             then Just (Pos (Ind baseReg) (fromIntegral offset))
                             else Just (Pos (Ind baseReg) (-(fromIntegral offset)))
                (1,0,0) -> let offset_ = bits (7, 0)
                               shiftReg = nthReg (bits (11, 8))
                          in if bit 37 == 0x00
                                then
                                Just $ BasShift baseReg  (ArithShiftL shiftReg (fromIntegral offset_))
                                else Just $ BasShift baseReg  (ArithShiftR shiftReg (fromIntegral offset_))
                (1,0,1) -> let shiftReg = nthReg (bits (11, 8))
                          in Just $ BasShift baseReg (Reg shiftReg)
                (1,1,1) -> let offset_ = bits (7, 0)
                               shiftReg = nthReg (bits (11, 8))
                          in if bit 37 == 0x00
                                then Just $ BasShift baseReg  (BinShiftL shiftReg (fromIntegral offset_))
                                else Just $ BasShift baseReg  (BinShiftR shiftReg (fromIntegral offset_))

     in op2 >>= (\op2' -> Just (instr (Reg destReg) op2'))

----------------------------------------------------------------------
-- Split a word into fields.
----------------------------------------------------------------------
splitWord
  :: Word64
  -> (Int, Int)
  -> Word64

splitWord word (hi, lo)
  = let mask = (2 ^ (hi - lo + 1) - 1) `shiftL` lo
    in (word .&. mask) `shiftR` lo



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
