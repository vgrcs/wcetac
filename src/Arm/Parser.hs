----------------------------------------------------------------------
-- FILE:              Parser.hs
-- DESCRIPTION:       Parser for ARM assembly programs.
-- DATE:              04/01/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: Hugs
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Parser
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Arm.ParseLib
import Data.Word
import Data.Char
import Control.Monad



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.BinaryNumber
import Arm.Instruction
import qualified Analyzer.LRU as LRU
import Arm.Operand
import Arm.Program
import Arm.RegisterName



----------------------------------------------------------------------
-- Type aliases.
----------------------------------------------------------------------
type Symbol = String

format places char str
  = let pad = places - (length str)
        fill = (take (pad `div` 2) (repeat  char))
        str' = fill ++ str ++ fill
    in if length str' `mod` 2 == 0 then str' else str'  ++ [' ']

----------------------------------------------------------------------
-- Parse element data type.
----------------------------------------------------------------------
data ParseElement
  = Data        String Constant
  | Prop        [ParseElement]
  | Instruction Instruction
  | Symbol      Symbol -- ParseElement
  | Comment
  | Newline
  | PropSection  [ParseElement]
  | DotProp      String [Constant]
  | CodeSection  ParseElement [ParseElement]
  --deriving (Show)

instance Show ParseElement where
  show (PropSection  xs) = "\n" ++ (format 100 '=' "Begin Prop section") ++ "\n" ++
                            show xs ++ "\n" ++
                           (format 100 '=' "End Prop section") ++ "\n"
  show (DotProp s cs) = "<<dot prop>> " ++ s ++ " " ++ show cs ++ "\n"
  show (Instruction i) = show i ++ "\n"
  show (Data op1 op2) = "<<data>> " ++  show op1 ++ " = " ++ show op2
  show (CodeSection elem xs) = "\n" ++ (format 100 '=' "Begin Code section") ++ "\n" ++
                                show elem ++ "\n" ++ show xs ++ "\n" ++
                               (format 100 '=' "End Code section") ++ "\n"
  show Newline = "\n"
  show (Prop xs) = "<<prop>> " ++ show xs ++ "\n"
  show (Symbol s) = "<<symbol>> " ++ s
  show Comment = "comment"




----------------------------------------------------------------------
-- This parses any number of spaces or tabs.  (``spaces'' parses at
-- least 1 space, and includes all white space characters including \n)
----------------------------------------------------------------------
spaces'
  = many (char ' ' +++ char '\t')



----------------------------------------------------------------------
-- Parse a comma which separates two values.  It can have any number
-- of spaces surrounding it.
----------------------------------------------------------------------
csep
   = do spaces'
        char ','
        spaces'
        return ()

sep c
  = do spaces'
       char c
       spaces'
       return ()



----------------------------------------------------------------------
-- Parse a 32-bit decimal word.
----------------------------------------------------------------------
pWord :: Parser Word32
pWord
  = do { x <- digit; return (fromIntegral (digitToInt x)) } `chainl1` return op
    where
      op :: Word32 -> Word32 -> Word32
      m `op` n = 10*m + n

pIntNeg :: Parser Constant
pIntNeg
  = do char '-'
       Int i <- pInt
       return (Int (-i))

pWordNeg :: Parser Word32
pWordNeg
  = do char '-'
       w <- pWord
       return (fromIntegral (-w) :: Word32)

 {-do {char '-'; pWord } `chainl1` return op
    where
      op :: Word32 -> Word32 -> Word32
      m `op` n = 10*m + n-}



----------------------------------------------------------------------
-- Parse a 32-bit hexadecimal word.
----------------------------------------------------------------------
hexDigit
  = sat isHexDigit

-- isHexDigit = (`elem` "0123456789abcdefABCDEF")

hexValue '0' = 0
hexValue '1' = 1
hexValue '2' = 2
hexValue '3' = 3
hexValue '4' = 4
hexValue '5' = 5
hexValue '6' = 6
hexValue '7' = 7
hexValue '8' = 8
hexValue '9' = 9
hexValue 'a' = 10
hexValue 'b' = 11
hexValue 'c' = 12
hexValue 'd' = 13
hexValue 'e' = 14
hexValue 'f' = 15
hexValue 'A' = 10
hexValue 'B' = 11
hexValue 'C' = 12
hexValue 'D' = 13
hexValue 'E' = 14
hexValue 'F' = 15

pHex'
  = do { x <- hexDigit; return (hexValue x) }
    `chainl1` return op
  where
    op :: Word32 -> Word32 -> Word32
    m `op` n = 16*m + n

pHex
  = do string "0x"
       pHex'



----------------------------------------------------------------------
-- Parse a binary word.
----------------------------------------------------------------------
pBinary
  :: Parser Word32

pBinary
  = do string "0b"
       bits <- many (char '0' +++ char '1')
       let bn = read bits
       return (binary32ToWord32 bn)



----------------------------------------------------------------------
-- Parse an integer, either hex or decimal.
----------------------------------------------------------------------
pIntegral
  = pHex +++ pBinary +++ pWord +++ pWordNeg



----------------------------------------------------------------------
-- Parse a newline.
----------------------------------------------------------------------
pNl
  = do spaces'
       optional (char '\r')  -- Windows puts a \r before the \n
       char '\n'
       return Newline


----------------------------------------------------------------------
-- Operand parsers.
----------------------------------------------------------------------

-- auto-indexed
pAut :: Parser Operand
pAut
  = do { b <- pBas; char '!'; return (Aut b) }
    +++ do { b <- pReg; char '!'; return (Aut b) }

-- base + offset
pBas :: Parser Operand
pBas
  = (do { char '['; (Reg r) <- pReg; csep;  rs <- pRegShift; char ']'; return (BasShift r rs) })
    +++
    (do { char '['; (Reg r1) <- pReg; csep;  r2 <- pReg; char ']'; return (BasShift r1 r2) })
    +++
    (do { char '['; (Reg r) <- pReg; csep;  Rel c <- pRel2; char ']'; return (Bas r (fromIntegral c)) })


-- constant
pCon :: Parser Operand
pCon
  = (do char '#' >> pIntegral >>= \w -> return (Con (fromIntegral w)))

-- relative offset
pRel2
   = (do { char '#'; i <- int; return (Rel i) })

-- indirect
pInd :: Parser Operand
pInd
  = do { char '['; Reg r <- pReg; char ']'; return (Ind r) }

-- multiple register
pMrg
  = do char '{'
       regs <- pMrg'
       regs' <- many (do { spaces'; char ','; spaces'; pMrg' })
       char '}'
       return (Mrg (foldl (++) [] (regs : regs')))
  where
    pMrg'
      = pRegRange
        +++ (do Reg r <- pReg
                return [r])
    pRegRange
      = do Reg r1 <- pReg
           char '-'
           Reg r2 <- pReg
           return (enumFromTo r1 r2)

-- post-indexed
pPos :: Parser Operand
pPos
  = --do { char '['; Reg r <- pReg; char ']'; csep; Con c <- pCon; return (Pos (Ind r) c) }
    do { char '['; Reg r <- pReg; char ']'; csep; Rel c <- pRel2; return (Pos (Ind r) (fromIntegral c)) }

-- register
pReg :: Parser Operand
pReg
  = (do char 'r'
        i <- nat
        if or [i < 0, i > 15]
           then mzero
           else return (Reg (nthReg (fromIntegral i))))
    +++ (do string "sp"
            return (Reg SP))
    +++ (do string "ip"
            return (Reg IP))
    +++ (do string "fp"
            return (Reg FP))
    +++ (do string "lr"
            return (Reg LR))
    +++ (do string "pc"
            return (Reg PC))
    +++ (do string "sl"
            return (Reg SL))

-- relative offset
pRel
   = (do { i <- int; return (Rel i) })

-- parse an operand
pOperand :: Parser Operand
pOperand
  = pAut +++ pBas +++ pCon +++ pPos +++ pInd +++ pRegShift
    +++ pReg +++ pRel +++ pMrg +++ pBranchLabel2


pRegShift :: Parser Operand
pRegShift
  = (do Reg r <- pReg
        csep
        string "asl"
        spaces'
        Rel c <- pRel2
        return (ArithShiftL r (fromIntegral c)))
    +++
    (do Reg r <- pReg
        csep
        string "asr"
        spaces'
        Rel c <- pRel2
        return (ArithShiftR r (fromIntegral c)))
    +++
    (do Reg r <- pReg
        csep
        string "lsr"
        spaces'
        Rel c <- pRel2
        return (BinShiftR r (fromIntegral c)))
    +++
    (do Reg r <- pReg
        csep
        string "lsl"
        spaces'
        Rel c <- pRel2
        return (BinShiftL r (fromIntegral c)))
    +++
    (do Reg r1 <- pReg
        csep
        string "asl"
        spaces'
        Reg r2 <- pReg
        return (ArithRegShiftL r1 r2))
    +++
    (do Reg r1 <- pReg
        csep
        string "asr"
        spaces'
        Reg r2 <- pReg
        return (ArithRegShiftR r1 r2))


----------------------------------------------------------------------
-- Parse two operands.
----------------------------------------------------------------------
p2Ops
  = do { op1 <- pOperand; csep; op2 <- pOperand; return (op1, op2) }



----------------------------------------------------------------------
-- Parse three operands.
----------------------------------------------------------------------
p3Ops
  = do { op1 <- pOperand; csep; op2 <- pOperand; csep; op3 <- pOperand; return (op1, op2, op3) }

----------------------------------------------------------------------
-- Parse four operands.
----------------------------------------------------------------------
p4Ops
  = do { op1 <- pOperand; csep; op2 <- pOperand; csep; op3 <- pOperand; csep; op4 <- pOperand; return (op1, op2, op3, op4) }

----------------------------------------------------------------------
-- Instruction parsers.
----------------------------------------------------------------------
pAdd   = ops3 "add"   Add
pAnd   = ops3 "and"   And
pB     = ops1 "b"     B
pAdr   = ops2 "adr"   Adr
pBeq   = ops1 "beq"   Beq
pBgt   = ops1 "bgt"   Bgt
pBic   = ops3 "bic"   Bic
pBcc   = ops1 "bcc"   Bcc
pBl    = ops1 "bl"    Bl
pBx    = ops1 "bx"    Ann1
pBlt   = ops1 "blt"   Blt
pBne   = ops1 "bne"   Bne
pBge   = ops1 "bge"   Bge
pBle   = ops1 "ble"   Ble
pBls   = ops1 "bls"   Bls
pBhi   = ops1 "bhi"   Bhi
pCmp   = ops2 "cmp"   Cmp
pEor   = ops3 "eor"   Eor
pLdmia = ops2 "ldmia" Ldmia
pLdmfd = ops2 "ldmfd" Ldmfd
pLdrls = ops2 "ldrls" Ldrls
pLdrsh = ops2 "ldrsh" Ldrsh
pMvn   = ops2 "mvn"   Mvn
pMul   = ops3 "mul"   Mul
pSMul  = ops4 "smull" SMul
pOrr   = ops3 "orr"   Orr
pStmfd = ops2 "stmfd" Stmfd
pStmia = ops2 "stmia" Stmia
pStrb  = ops2 "strb"  Strb
pSub   = ops3 "sub"   Sub
pRsb   = ops3 "rsb"   Rsb
pSwi   = ops1 "swi"   Swi
pSave  = ops1 ".save" Ann1
pSetFP = ops3 ".setfp" Ann3
pPad   = ops1 ".pad" Ann1

pLdrb  = (do
          string "ldrb"
          spaces
          (op1, op2) <- p2Ops
          char '\t'
          char '@'
          char ' '
          string "zero_extendqisi2"
          return (Instruction (Ldrb op1 op2)) )
         +++
         (do
          string "ldrb"
          spaces
          (op1, op2) <- p2Ops
          return (Instruction (Ldrb op1 op2)) )

pStrh  = do
         string "strh"
         spaces
         (op1, op2) <- p2Ops
         spaces'
         char '@'
         spaces'
         string "movhi"
         return (Instruction (Strh op1 op2))


pLdrh   = (do i <- (ops2 "ldrh"   Ldrh)
              char '\t'
              char '@'
              char ' '
              string "movhi"
              return i)
          +++
          ops2 "ldrh"   Ldrh

pMov   = (do i <- (ops2 "mov"   Mov)
             char '\t'
             char '@'
             char ' '
             string "movhi"
             return i)
         +++
         (ops2' "mov"   Mov)
         +++
         (ops2 "mov"   Mov)
         +++
         (ops2 "movlt"   Movlt)
         +++
         (ops2 "movne"   Movne)
         +++
         (ops2 "moveq"   Moveq)
         +++
         (ops2 "movls"   Movls)
         +++
         (ops2 "movhi"   Movhi)
         +++
         (ops2 "movge"   Movge)
         +++
         (ops2 "movcs"   Movcs)
         +++
         (ops2 "movcc"   Movcc)

pLdr   = (do i <- (ops2 "ldr"   Ldr)
             char '\t'
             char '@'
             char ' '
             string "float"
             return i)
         +++
         (ops2 "ldr"   Ldr)

pStr   = (do i <- (ops2 "str"   Str)
             char '\t'
             char '@'
             char ' '
             string "float"
             return i)
         +++
         (ops2 "str"   Str)

----------------------------------------------------------------------
-- Instruction meta-parsers.
----------------------------------------------------------------------
-- instruction with one operand
ops1 name instr
  = do
    string name
    spaces
    op1 <- pOperand
    case op1 of
         Lab "printf" -> return (Instruction (Printf (Con 10)))
         Lab "pthread_exit" -> return (Instruction (PThreadExit (Con 10)))
         Lab "pthread_create" -> return (Instruction (PThreadCreate (Con 10)))
         Lab "exit" -> return (Instruction (Exit (Con 10)))
         Lab "pthread_mutex_lock" -> return (Instruction (PthreadMutexLock (Con 10)))
         Lab "pthread_cond_wait" -> return (Instruction (PthreadCondWait (Con 10)))
         Lab "pthread_mutex_unlock" -> return (Instruction (PthreadMutexUnlock (Con 10)))
         Lab "pthread_mutex_destroy" -> return (Instruction (PthreadMutexDestroy (Con 10)))
         Lab "pthread_cond_signal" -> return (Instruction (PthreadCondSignal (Con 10)))
         Lab "pthread_mutexattr_init" -> return (Instruction (PthreadMutexattrInit (Con 10)))
         Lab "pthread_mutexattr_setpshared" -> return (Instruction (PthreadMutexattrSetpshared (Con 10)))
         Lab "shm_open" -> return (Instruction (ShmOpen (Con 10)))
         Lab "shm_unlink" -> return (Instruction (ShmUnlink (Con 10)))
         Lab "ftruncate" -> return (Instruction (Ftruncate (Con 10)))
         Lab "mmap" -> return (Instruction (Mmap (Con 10)))
         Lab "munmap" -> return (Instruction (Munmap (Con 10)))
         Lab "pthread_mutex_init" -> return (Instruction (PthreadMutexInit (Con 10)))
         Lab "pthread_mutexattr_destroy" -> return (Instruction (PthreadMutexattrDestroy (Con 10)))
         Lab "fork" -> return (Instruction (Fork (Con 10)))
         Lab "waitpid" -> return (Instruction (Waitpid (Con 10)))
         Lab "__divsi3" -> return (Instruction (Generic (Con 10)))
         Lab "__floatsidf" -> return (Instruction (Generic (Con 10)))
         Lab "__adddf3" -> return (Instruction (Generic (Con 10)))
         Lab "__fixdfsi" -> return (Instruction (Generic (Con 10)))
         Lab "__gedf2" -> return (Instruction (Generic (Con 10)))
         Lab "__divdf3" -> return (Instruction (Generic (Con 10)))
         Lab "__eqdf2" -> return (Instruction (Generic (Con 10)))
         Lab "__muldf3" -> return (Instruction (Generic (Con 10)))
         Lab "__subdf3" -> return (Instruction (Generic (Con 10)))
         Lab "__gtdf2" -> return (Instruction (Generic (Con 10)))
         Lab "__ledf2" -> return (Instruction (Generic (Con 10)))
         Lab "__umodsi3" -> return (Instruction (Generic (Con 10)))
         Lab "__nedf2" -> return (Instruction (Generic (Con 10)))
         _ -> return (Instruction (instr op1))

-- instruction with two operands
ops2 name instr
  = do { string name; spaces; (op1, op2) <- p2Ops; return (Instruction (instr op1 op2)) }

ops2' name instr
  = do { string name; spaces; (op1, op2,_,_) <- p4Ops ; return (Instruction (instr op1 op2)) }

-- instruction with three operands
ops3 name instr
  = do { string name; spaces; (op1, op2, op3) <- p3Ops; return (Instruction (instr op1 op2 op3)) }

-- instruction with three operands
ops4 name instr
  = do { string name; spaces; (op1, op2, op3,op4) <- p4Ops; return (Instruction (instr op1 op2 op3 op4)) }


----------------------------------------------------------------------
-- Parse an instruction.
----------------------------------------------------------------------
pInstr
  = pAdd +++ pAnd +++ pB +++ pBeq   +++ pBgt   +++ pBic   +++ pBl   +++ pBlt +++ pBge +++ pBle
         +++ pBcc +++ pBx
         +++ pBne +++ pCmp +++ pEor   +++ pLdmia +++ pLdr   +++ pLdrb +++ pMov +++ pMvn +++ pBls
         +++ pMul +++ pOrr +++ pStr   +++ pStrb  +++ pSub  +++ pSwi +++ pBhi
         +++ pLdmfd +++ pStmfd +++ pStmia +++ pRsb +++ pAdr +++ pSMul  +++ pStrh +++
         pLdrh +++ pLdrsh +++ pLdrls
         +++ pSave +++ pSetFP +++ pPad



----------------------------------------------------------------------
-- Parse a label.
----------------------------------------------------------------------
pLabel
  = (do l1 <- many1 letter
        char '.'
        l2 <- many1 digit
        char ':'
        return (Symbol (l1 ++ "." ++ l2)))
    +++
    (do l <- pLabel'
        char ':'
        return (Symbol l) )
    +++
    (do char '.'
        l <- pLabel'
        char ':'
        return (Symbol ("." ++ l)))


pBranchLabel2
  = (do l1 <- many1 letter
        char '_'
        l2 <- many1 letter
        char '_'
        l3 <- many1 letter
        return (Lab (l1 ++ "_" ++ l2 ++ "_" ++ l3)))
    +++
    (do l1 <- many1 letter
        char '_'
        l2 <- many1 letter
        return (Lab (l1 ++ "_" ++ l2)))
    +++
    (do l <- pLabel'
        return (Lab l))
    +++
    (do char '.'
        l <- pLabel'
        char '+'
        offset <- int
        return (LabRel ("." ++ l) offset))
    +++
    (do char '.'
        l <- pLabel'
        return (Lab ("." ++ l)))



pLabel'
  = do { xs <- many1 label; return xs }



----------------------------------------------------------------------
-- Parse a comment.
----------------------------------------------------------------------
pComment
  = do { char ';'; many (sat (\x -> x /= '\n')); return Comment }



----------------------------------------------------------------------
-- Return a parsed token in the list monad (optionally ``[]'')
----------------------------------------------------------------------
optional p
  = (do x <- p
        return [x])
    +++ return []


----------------------------------------------------------------------
-- Parse various constants for the data segment.
----------------------------------------------------------------------
pInt
  = int >>= (return . Int)

pChar
  = do char '\''
       c <- sat (\_ -> True)
       char '\''
       return (Int (toInteger (fromEnum c)))

pString
  = do char '"'
       s <- many (sat (\c -> c /= '"'))
       char '"'
       return (String s)

pArray
  = do string "array"
       spaces'
       n <- int
       spaces'
       c <- pData
       return (Array (fromIntegral n) c)



----------------------------------------------------------------------
-- Parse a single constant.
----------------------------------------------------------------------


dataSep = do { char '.' ; return () } +++ do { char ',' ; return () }

pData
  = (do w <- pIntegral
        return (Word w))
    +++ pInt
    +++ pChar
    +++ pString
    +++ pArray
    +++ (do c1 <- many1 letter
            char '.'
            c2 <- many1 digit
            return (String (c1 ++ "." ++ c2)))
    +++ (do c <- many1 label
            return (String c))
    +++ (do char '.'
            string "text"
            char '.'
            c <- many1 label
            return (String (".text." ++ c)))
    +++ (do char '.'
            string "note"
            char '.'
            c <- many1 label
            return (String (".node." ++ c)))
    +++ (do char '.'
            char '-'
            c <- many1 label
            return (String (".-" ++ c)))
    +++ (do char '%'
            c <- many1 label
            return (String ("%" ++ c)))
    +++ (do char '.'
            c1 <- many1 alphanum
            char '-'
            char '.'
            c2 <- many1 alphanum
            return (String ("." ++ c1 ++ "-." ++ c2)))
    +++ (do char '.'
            c <- many1 label
            return (String ("." ++ c)))
    +++ (do return (String "empty"))

----------------------------------------------------------------------
-- Parse a list of constants
----------------------------------------------------------------------
pDataList
  = (do c <- pData
        csep
        cs <- pDataList
        return (c : cs))
    +++ (do c <- pData
            return [c])

pGenericList p sep
  = (do c <- p
        sep
        cs <- pGenericList p sep
        return (c : cs))
    +++ (do c <- p
            return [c])


----------------------------------------------------------------------
-- Parse a line of the constant segment in a text file.
----------------------------------------------------------------------

pPropLines
  = (do spaces'
        char '@'
        spaces'
        Prop p <- pPropLine
        return p
        char '\n'
        Prop ps <- pPropLines
        return (Prop (p ++ ps)) )
    +++ (do spaces'
            char '@'
            spaces'
            p <- pPropLine
            return p)

pPropLine
  = (do spaces'
        l <- pProp
        csep
        spaces'
        ls <- pPropLine
        return (Prop (l:ls:[])))
    +++  (do l <- pProp
             return (Prop [l]))

pProp
  = do label <- (do l <- many1 prop
                    spaces'
                    char '='
                    return l)
                 +++
                 (do l <- string "Function supports interworking."
                     return l)
                 +++
                 (do l <- string "link register save eliminated."
                     return l)
       spaces'
       p <- pData
       return (Data ("@ " ++ label) p)


----------------------------------------------------------------------
-- Parse a section.
----------------------------------------------------------------------

pSection
  = (do --char '.'
        l <- pLabel
        pNl
        is <- pGenericList (spaces' >> pInstr) pNl
        pNl
        dotProps <- pHeader
        return (CodeSection (l) (is ++ dotProps:[])))
    +++
    (do l <- pLabel
        pNl
        atProps <- pPropLines
        pNl
        is <- pGenericList (spaces' >> pInstr) pNl
        pNl
        dotProps <- pHeader
        return (CodeSection l (atProps:[] ++ is ++ dotProps:[])))
    +++
    (do l <- pLabel
        pNl
        atProps <- pPropLines
        pNl
        is <- pGenericList (spaces' >> pInstr) pNl
        return (CodeSection l (atProps:[] ++ is)))
    +++

    (do --char '.'
        l <- pLabel
        pNl
        is <- pGenericList (spaces' >> pInstr) pNl
        return (CodeSection l is))
    +++
    (do l <- pLabel
        pNl
        dotProps <- pHeader
        return (CodeSection l (dotProps:[])))
    +++
    (do --char '.'
        l <- pLabel
        return (CodeSection (l) []))




----------------------------------------------------------------------
-- Parse the header.
----------------------------------------------------------------------

pHeader = do h <- pGenericList pHeaderLine pNl
             return (PropSection h)

pHeaderLine
  = do spaces'
       label <- pHeaderProp
       spaces'
       cs    <- pDataList
       return (DotProp label cs)

{-pHeaderProp
  = do char '.'
       l <- pLabel'
       return ("." ++ l)-}

pHeaderProp
   = (do l <- string ".file"
         return l)
     +++
     (do l <- string ".text"
         return l)
     +++
     (do l <- string ".data"
         return l)
     +++
     (do l <- string ".align"
         return l)
     +++
     (do l <- string ".global"
         return l)
     +++
     (do l <- string ".type"
         return l)
     +++
     (do l <- string ".size"
         return l)
     +++
     (do l <- string ".ident"
         return l)
     +++
     (do l <- string ".section"
         return l)
     +++
     (do l <- string ".ascii"
         return l)
     +++
     (do l <- string ".word"
         return l)
     +++
     (do l <- string ".short"
         return l)
     +++
     (do l <- string ".comm"
         return l)
     +++
     (do l <- string ".space"
         return l)
     +++
     (do l <- string ".byte"
         return l)
     +++
     (do l <- string ".local"
         return l)
     +++
     (do l <- string ".p2align"
         return l)
     +++
     (do l <- string ".bss"
         return l)
     +++
     (do l <- string ".weak"
         return l)
     +++
     (do l <- string ".uleb128"
         return l)
     +++
     (do l <- string ".arch"
         return l)
     +++
     (do l <- string ".fpu"
         return l)
     +++
     (do l <- string ".eabi_attribute"
         return l)
     +++
     (do l <- string ".fnstart"
         return l)
     +++
     (do l <- string ".personality"
         return l)
     +++
     (do l <- string ".handlerdata"
         return l)
     +++
     (do l <- string ".fnend"
         return l)
     +++
     (do l <- string ".cantunwind"
         return l)
     +++
     (do a <- pLabel'
         char ' '
         l <- string "="
         char ' '
         b <- pLabel'
         return (a ++ l ++ b))
----------------------------------------------------------------------
-- Parse a single program file element.
----------------------------------------------------------------------
pProgElem
  = do spaces'
       elem <- (pNl
                +++ pHeader
                +++ pSection
                )
       return elem



----------------------------------------------------------------------
-- Parse an entire program.
----------------------------------------------------------------------
pProgram :: Parser [ParseElement]

pProgram
  = do { elems <- many pProgElem; return elems }


----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
