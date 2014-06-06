----------------------------------------------------------------------
-- FILE:              Instruction.hs
-- DATE:              2/6/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Instruction
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Word
import Data.List


----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Operand
import Arm.RegisterName


----------------------------------------------------------------------
-- Instruciton data type.
----------------------------------------------------------------------
data Instruction
  = Add   Operand Operand Operand
  | And   Operand Operand Operand
  | B     Operand
  | Beq   Operand
  | Bgt   Operand
  | Bic   Operand Operand Operand
  | Bl    Operand
  | Blt   Operand
  | Bx    Operand
  | Bne   Operand
  | Bge   Operand
  | Ble   Operand
  | Bls   Operand
  | Blo   Operand
  | Bhs   Operand
  | Bhi   Operand
  | Bcc   Operand
  | Adr   Operand Operand
  | Cmp   Operand Operand
  | Eor   Operand Operand Operand
  | Ldmia Operand Operand
  | Ldmfd Operand Operand
  | Ldr   Operand Operand
  | Ldrls Operand Operand
  | Ldrb  Operand Operand
  | Ldrh  Operand Operand
  | Ldrsh Operand Operand
  | Mov   Operand Operand
  | Movlt Operand Operand
  | Movne Operand Operand
  | Moveq Operand Operand
  | Movge Operand Operand
  | Movls Operand Operand
  | Movhi Operand Operand
  | Movcs Operand Operand
  | Movcc Operand Operand
  | Mvn   Operand Operand
  | Mul   Operand Operand Operand
  | SMul  Operand Operand Operand Operand
  | Orr   Operand Operand Operand
  | Stmfd Operand Operand
  | Stmia Operand Operand
  | Str   Operand Operand
  | Strb  Operand Operand
  | Strh  Operand Operand
  | Strsh Operand Operand
  | Sub   Operand Operand Operand
  | Rsb   Operand Operand Operand
  | Swi   Operand
  | Nop
  | Ann1  Operand
  | Ann2  Operand Operand
  | Ann3  Operand Operand Operand

  | Printf Operand
  | Const Operand
  | PThreadCreate Operand
  | PThreadExit Operand
  | Exit Operand

  | PthreadMutexLock Operand
  | PthreadCondWait Operand
  | PthreadMutexUnlock Operand
  | PthreadMutexDestroy Operand
  | PthreadCondSignal Operand
  | PthreadMutexattrInit Operand
  | PthreadMutexattrSetpshared Operand
  | ShmOpen Operand
  | ShmUnlink Operand
  | Ftruncate Operand
  | Mmap Operand
  | Munmap Operand
  | PthreadMutexInit Operand
  | PthreadMutexattrDestroy  Operand
  | Fork Operand
  | Waitpid Operand
  | Generic Operand
  deriving Eq


instance Show Instruction where
  show (Add   op1 op2 op3) = "add   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (And   op1 op2 op3) = "and   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (B     op1)         = "b     " ++ show op1
  show (Beq   op1)         = "beq   " ++ show op1
  show (Bgt   op1)         = "bgt   " ++ show op1
  show (Bic   op1 op2 op3) = "bic   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Bl    op1)         = "bl    " ++ show op1
  show (Blt   op1)         = "blt   " ++ show op1
  show (Bx    op1)         = "bx    " ++ show op1
  show (Bne   op1)         = "bne   " ++ show op1
  show (Bge   op1)         = "bge   " ++ show op1
  show (Ble   op1)         = "ble   " ++ show op1
  show (Blo   op1)         = "blo   " ++ show op1
  show (Bls   op1)         = "bls   " ++ show op1
  show (Bhi   op1)         = "bhi   " ++ show op1
  show (Bhs   op1)         = "bhs   " ++ show op1
  show (Bcc   op1)         = "bcc   " ++ show op1
  show (Cmp   op1 op2)     = "cmp   " ++ show op1 ++ ", " ++ show op2
  show (Eor   op1 op2 op3) = "eor   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Ldmia op1 op2)     = "ldmia " ++ show op1 ++ ", " ++ show op2
  show (Ldmfd op1 op2)     = "ldmfd " ++ show op1 ++ ", " ++ show op2
  show (Ldr   op1 op2)     = "ldr   " ++ show op1 ++ ", " ++ show op2
  show (Ldrls op1 op2)     = "ldrls " ++ show op1 ++ ", " ++ show op2
  show (Ldrb  op1 op2)     = "ldrb  " ++ show op1 ++ ", " ++ show op2
  show (Ldrh  op1 op2)     = "ldrh  " ++ show op1 ++ ", " ++ show op2
  show (Ldrsh op1 op2)     = "ldrsh " ++ show op1 ++ ", " ++ show op2
  show (Mov   op1 op2)     = "mov   " ++ show op1 ++ ", " ++ show op2
  show (Movlt op1 op2)     = "movlt " ++ show op1 ++ ", " ++ show op2
  show (Movne op1 op2)     = "movne " ++ show op1 ++ ", " ++ show op2
  show (Movge op1 op2)     = "movge " ++ show op1 ++ ", " ++ show op2
  show (Moveq op1 op2)     = "moveq " ++ show op1 ++ ", " ++ show op2
  show (Movls op1 op2)     = "movls " ++ show op1 ++ ", " ++ show op2
  show (Movcs op1 op2)     = "movcs " ++ show op1 ++ ", " ++ show op2
  show (Movhi op1 op2)     = "movhi " ++ show op1 ++ ", " ++ show op2
  show (Movcc op1 op2)     = "movcc " ++ show op1 ++ ", " ++ show op2
  show (Mvn   op1 op2)     = "mvn   " ++ show op1 ++ ", " ++ show op2
  show (Mul   op1 op2 op3) = "mul   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (SMul  op1 op2 op3 op4) = "smul  " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3 ++ ", " ++ show op4
  show (Orr   op1 op2 op3) = "orr   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Stmfd op1 op2)     = "stmfd " ++ show op1 ++ ", " ++ show op2
  show (Stmia op1 op2)     = "stmia " ++ show op1 ++ ", " ++ show op2
  show (Str   op1 op2)     = "str   " ++ show op1 ++ ", " ++ show op2
  show (Strb  op1 op2)     = "strb  " ++ show op1 ++ ", " ++ show op2
  show (Strh  op1 op2)     = "strh  " ++ show op1 ++ ", " ++ show op2
  show (Sub   op1 op2 op3) = "sub   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Rsb   op1 op2 op3) = "rsb   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Swi   op1)         = "swi   " ++ show op1
  show (Nop)               = "nop   "
  show (Printf op1)        = "bl printf " ++ show op1
  show (PThreadCreate op1) = "bl pthread_create " ++ show op1
  show (PThreadExit op1)   = "bl pthread_exit " ++ show op1
  show (Exit op1)          = "bl exit " ++ show op1
  show (PthreadMutexLock op1) = "bl pthread_mutex_lock " ++ show op1
  show (PthreadCondWait op1) = "bl pthread_cond_wait " ++ show op1
  show (PthreadMutexUnlock op1) = "bl pthread_mutex_unlock " ++ show op1
  show (PthreadMutexDestroy op1) = "bl pthread_mutex_destroy " ++ show op1
  show (PthreadCondSignal op1) = "bl pthread_cond_signal " ++ show op1
  show (PthreadMutexattrInit op1) = "bl pthread_mutexattr_init " ++ show op1
  show (PthreadMutexattrSetpshared op1) = "bl pthread_mutexattr_setshared " ++ show op1
  show (ShmOpen op1) = "bl shm_open " ++ show op1
  show (ShmUnlink op1) = "bl shm_unlink " ++ show op1
  show (Ftruncate op1) = "bl ftruncate " ++ show op1
  show (Mmap op1) = "bl mmap " ++ show op1
  show (Munmap op1) = "bl munmap " ++ show op1
  show (PthreadMutexInit op1) = "bl pthread_mutex_init " ++ show op1
  show (PthreadMutexattrDestroy  op1) = "bl pthread_mutexattr_destroy " ++ show op1
  show (Fork op1) = "bl fork " ++ show op1
  show (Waitpid op1) = "bl waitpid " ++ show op1
  show (Generic op1) = "bl __system " ++ show op1
  show (Adr op1 op2) = "adr   " ++ show op1 ++ ", " ++ show op2
  show (Ann1 _ )     = "annotation"
  show (Ann2 _ _)    = "annotation"
  show (Ann3 _ _ _)  = "annotation"


-- | Get the names of the operands involved
names
  :: Instruction
  -> [RegisterName]

names (Add (Reg reg1) (Reg reg2) (Con c)) = nub [reg1, reg2]
names (And (Reg reg1) (Reg reg2) (Con c)) = nub [reg1, reg2]
names (Orr (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (Eor (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (Add (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (Mul (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (SMul (Reg reg1) (Reg reg2) (Reg reg3) (Reg reg4)) = nub [reg1, reg2, reg3, reg4]
names (Sub (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (Rsb (Reg reg1) (Reg reg2) (Reg reg3)) = nub [reg1, reg2, reg3]
names (Rsb (Reg reg1) (Reg reg2) (Con c)) = nub [reg1, reg2]
names (Sub (Reg reg1) (Reg reg2) (Con c)) = nub [reg1, reg2]
names (Mov (Reg reg1) (Con con)) = [reg1]
names (Movne (Reg reg1) (Con con)) = [reg1, CPSR]
names (Movls (Reg reg1) (Con con)) = [reg1, CPSR]
names (Moveq (Reg reg1) (Con con)) = [reg1, CPSR]
names (Movge (Reg reg1) (Reg reg2)) = [reg1, reg2, CPSR]
names (Movhi (Reg reg1) (Con con)) = [reg1, CPSR]
names (Mov (Reg reg1) (Reg reg2)) = [reg1, reg2]
names (Mov (Reg reg1) (ArithShiftL reg2 _)) = [reg1, reg2]
names (Mov (Reg reg1) (ArithShiftR reg2 _)) = [reg1, reg2]
names (Mov (Reg reg1) (ArithRegShiftL reg2 reg3)) = [reg1, reg2, reg3]
names (Mov (Reg reg1) (ArithRegShiftR reg2 reg3)) = [reg1, reg2, reg3]
names (Mov (Reg reg1) (BinShiftL reg2 _)) = [reg1, reg2]
names (Mov (Reg reg1) (BinShiftR reg2 _)) = [reg1, reg2]
names (Mvn (Reg reg1) (Con con)) = [reg1]
names (Mvn (Reg reg1) (Reg reg2)) = [reg1, reg2]
names (Beq _) = [CPSR]
names (Bne _) = [CPSR]
names (Bge _) = [CPSR]
names (Ble _) = [CPSR]
names (Bgt _) = [CPSR]
names (Blt _) = [CPSR]
names (Blo _) = [CPSR]
names (Bls _) = [CPSR]
names (Bhi _) = [CPSR]
names (Bhs _) = [CPSR]
names (Bx (Reg reg1)) = [reg1]
names (B _) = []
names (Bl _) = []
names (Cmp (Reg reg1) (Con c)) = [reg1, CPSR]
names (Cmp (Reg reg1) (Reg reg2)) = nub [reg1, reg2, CPSR]
names (Str (Reg reg1) (Ind reg2))  = nub [reg1, reg2]
names (Str (Reg reg1) (Bas reg2 _))  = nub [reg1, reg2]
names (Strh (Reg reg1) (Bas reg2 _))  = nub [reg1, reg2]
names (Strh (Reg reg1) (Ind reg2))  = nub [reg1, reg2]
names (Str (Reg reg1) (BasShift baseReg (ArithShiftL shiftReg bits)))  = nub [reg1, baseReg, shiftReg]
names (Strb (Reg reg1) (BasShift baseReg (Reg shiftReg)))  = nub [reg1, baseReg, shiftReg]
names (Strb (Reg reg1) (Bas baseReg _))  = nub [reg1, baseReg]
names (Strb (Reg reg1) (Ind reg2))  = nub [reg1, reg2]
names (Swi _)  = []
names (Nop)  = []
names (Printf _)  = []
names (PThreadCreate _)  = []
names (PThreadExit _)  = []
names (Exit _)  = []
names (Ldr (Reg reg1) (Con _)) = [reg1]
names (Ldr (Reg reg1) (Ind reg2)) = nub [reg1, reg2]
names (Ldr (Reg reg1) (Bas reg2 _)) = nub [reg1, reg2]
names (Ldr (Reg reg1) (Aut (Bas reg2 _))) = nub [reg1, reg2]
names (Ldr (Reg reg1) (BasShift r1 (ArithShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldr (Reg reg1) (BasShift r1 (ArithShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldr (Reg reg1) (BasShift r1 (BinShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldr (Reg reg1) (BasShift r1 (BinShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldr (Reg reg1) (BasShift r1 (Reg r2))) = nub [reg1, r1, r2]

names (Ldrb (Reg reg1) (Con _)) = [reg1]
names (Ldrb (Reg reg1) (Ind reg2)) = nub [reg1, reg2]
names (Ldrb (Reg reg1) (Bas reg2 _)) = nub [reg1, reg2]
names (Ldrb (Reg reg1) (Aut (Bas reg2 _))) = nub [reg1, reg2]
names (Ldrb (Reg reg1) (BasShift r1 (ArithShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldrb (Reg reg1) (BasShift r1 (ArithShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldrb (Reg reg1) (BasShift r1 (BinShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldrb (Reg reg1) (BasShift r1 (BinShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldrb (Reg reg1) (BasShift r1 (Reg r2))) = nub [reg1, r1, r2]

names (Ldrh (Reg reg1) (Con _)) = [reg1]
names (Ldrh (Reg reg1) (Ind reg2)) = nub [reg1, reg2]
names (Ldrh (Reg reg1) (Bas reg2 _)) = nub [reg1, reg2]
names (Ldrh (Reg reg1) (Aut (Bas reg2 _))) = nub [reg1, reg2]
names (Ldrh (Reg reg1) (BasShift r1 (ArithShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldrh (Reg reg1) (BasShift r1 (ArithShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldrh (Reg reg1) (BasShift r1 (BinShiftL r2 _))) = nub [reg1, r1, r2]
names (Ldrh (Reg reg1) (BasShift r1 (BinShiftR r2 _))) = nub [reg1, r1, r2]
names (Ldrh (Reg reg1) (BasShift r1 (Reg r2))) = nub [reg1, r1, r2]

names (Ldrsh (Reg reg1) (Bas reg2 _)) =  nub  ([reg1, reg2])
names (Stmfd op1 (Mrg regList)) =  let r = case op1 of
                                                Aut (Reg r) -> r
                                                Reg r -> r
                                   in r : regList
names (Stmia op1 (Mrg regList)) =  let r = case op1 of
                                                Aut (Reg r) -> r
                                                Reg r -> r
                                   in r : regList
names (Ldmfd op1 (Mrg regList)) =  let r = case op1 of
                                                Aut (Reg r) -> r
                                                Reg r -> r
                                   in r : regList

names (PthreadMutexLock  _) = []
names (PthreadCondWait  _) = []
names (PthreadMutexUnlock  _) = []
names (PthreadMutexDestroy  _) = []
names (PthreadCondSignal  _) = []
names (PthreadMutexattrInit  _) = []
names (PthreadMutexattrSetpshared  _) = []
names (ShmOpen  _) = []
names (ShmUnlink  _) = []
names (Ftruncate  _) = []
names (Mmap  _) = []
names (Munmap  _) = []
names (PthreadMutexInit  _) = []
names (PthreadMutexattrDestroy   _) = []
names (Fork  _) = []
names (Waitpid  _) = []
names (Generic  _) = [R13,R11]



names other = error $ "names " ++ (show other)



accessMemory ::
  Instruction
  -> Bool

accessMemory (Str _ _) = True
accessMemory (Ldr _ _) = True
accessMemory (Stmfd _ _) = True
accessMemory (Ldmfd _ _) =  True
accessMemory _ = False


complInstr (Bgt a) = (Ble a)
complInstr (Ble a) = (Bgt a)

complInstr (Blt a) = (Bge a)
complInstr (Bge a) = (Blt a)

complInstr (Beq a) = (Bne a)
complInstr (Bne a) = (Beq a)

----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
