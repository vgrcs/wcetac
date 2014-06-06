----------------------------------------------------------------------
-- FILE:              Operand.hs
-- DATE:              02/17/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Operand
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.RegisterName
import Data.Int


----------------------------------------------------------------------
-- Operand data type.
----------------------------------------------------------------------
data Operand
  = Aut Operand                 -- auto-increment
  | Bas RegisterName Integer    -- base + offset
  | BasShift RegisterName Operand    -- base + shift register
  | Con Word32                  -- constant
  | Ind RegisterName            -- indirect
  | Mrg [RegisterName]          -- multiple register
  | Pos Operand Integer         -- post-indexed
  | Reg RegisterName            -- register
  | Rel Integer                 -- relative address
  | Lab String                  -- for parsing branches
  | LabRel String Integer       -- for parsing access to data
  | ArithShiftL RegisterName Integer
  | ArithShiftR RegisterName Integer
  | ArithRegShiftL RegisterName RegisterName
  | ArithRegShiftR RegisterName RegisterName
  | BinShiftL RegisterName Integer
  | BinShiftR RegisterName Integer


instance Show Operand where
  show (Aut op)      = show op ++ "!"
  show (Bas reg off) = "[" ++ show reg ++ ", #" ++ show off ++ "]"
  show (BasShift reg off) = "[" ++ show reg ++ ", " ++ show off ++ "]"
  show (Con wrd)     = "#" ++ show wrd
  show (Ind reg)     = "[" ++ show reg ++ "]"
  show (Lab lab)     = lab
  show (Mrg regs)    = "{" ++ showMrg regs ++ "}"
  show (Pos op off)  = show op ++ ", #" ++ show off
  show (Reg reg)     = show reg
  show (Rel rel)     = show rel
  show (LabRel lab rel) = lab ++ "+" ++ show rel
  show (ArithShiftL reg off) = show reg ++ ", asl #" ++ show off
  show (ArithShiftR reg off) = show reg ++ ", asr #" ++ show off
  show (ArithRegShiftL reg r) = show reg ++ ", asl " ++ show r
  show (ArithRegShiftR reg r) = show reg ++ ", asr " ++ show r
  show (BinShiftL reg off) = show reg ++ ", lsl #" ++ show off
  show (BinShiftR reg off) = show reg ++ ", lsr #" ++ show off

  --showsPrec 1 (Con wrd)     = (("\\#" ++ show wrd) ++)

instance Eq Operand where
  (Aut op1) == (Aut op2)      = op1 == op2
  (Bas reg1 off1) == (Bas reg2 off2) = reg1 == reg2 && off1 == off2
  (BasShift reg1 off1) == (BasShift reg2 off2) = reg1 == reg2 && off1 == off2
  (Con wrd1) == (Con wrd2)     = wrd1 == wrd2
  (Ind reg1) == (Ind reg2)     = reg1 == reg2
  (Lab lab1) == (Lab lab2)     = lab1 == lab2
  (Mrg regs1) == (Mrg regs2)   = regs1 == regs2
  (Pos op1 off1) == (Pos op2 off2)  = op1 == op2 && off1 == off2
  (Reg reg1) == (Reg reg2)     = reg1 == reg2
  (Rel rel1) == (Rel rel2)     = rel1 == rel2
  (Bas reg1 0) == (Ind reg2)   = reg1 == reg2
  (Ind reg2)  == (Bas reg1 0)  = reg1 == reg2
  (ArithShiftL reg1 off1) == (ArithShiftL reg2 off2) = reg1 == reg2 && off1 == off2
  (ArithShiftR reg1 off1) == (ArithShiftR reg2 off2) = reg1 == reg2 && off1 == off2
  (ArithRegShiftL reg1 reg2) == (ArithRegShiftL reg3 reg4) = reg1 == reg3 && reg2 == reg4
  (ArithRegShiftR reg1 reg2) == (ArithRegShiftR reg3 reg4) = reg1 == reg3 && reg2 == reg4
  (BinShiftL reg1 off1) == (BinShiftL reg2 off2) = reg1 == reg2 && off1 == off2
  (BinShiftR reg1 off1) == (BinShiftR reg2 off2) = reg1 == reg2 && off1 == off2
  a == b = False

showMrg []       = ""
showMrg [r]      = show r
showMrg (r : rs) = show r ++ "," ++ showMrg rs



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
