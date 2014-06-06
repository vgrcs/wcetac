module Arm.Assembler
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Prelude
import Data.Word
import Data.Char
import Data.Bits
import Data.Int
import Data.List
import Control.Monad
import qualified Data.Map as Map
import System.IO.Unsafe

----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Loader
import Arm.Operand
import Arm.ParseLib
import Arm.Parser hiding (Data)
import Arm.Program
import Arm.RegisterName



----------------------------------------------------------------------
-- Result data type.
----------------------------------------------------------------------
data AsmResult
  = Res Program
  | Err String
  deriving (Show)



----------------------------------------------------------------------
-- Expand instruction macros.  (currently there are none)
----------------------------------------------------------------------
expandMacros l = l



showSymbols
  :: (String, (Word32, SectionType))
  -> (String, (String, SectionType))

showSymbols (name, (val, ty))
  = (name, (wordToString val, ty))

----------------------------------------------------------------------
-- Resolve labels in a program.
----------------------------------------------------------------------
resolveSymbols
  :: Word32 -- program counter
  -> Int -- constants
  -> [ParseElement]
  -> IO [(String, (Word32, SectionType))]

resolveSymbols _ _ []
  = return []

resolveSymbols pc cons (CodeSection label instrs : rest)
  = do resolveSymbols pc cons (label:instrs ++ rest)

resolveSymbols pc cons (Symbol l : (PropSection sec) :rest)
  = do (cons' , s1) <- resolveSymbols' l pc cons sec
       let detect (_,(_,x)) = case x of {Byte _ -> True; _ -> False}
           bytes = filter detect s1
           left = foldl (\accum s -> delete s accum) s1 bytes
           pack [] accum
              = accum
           pack list accum | (length list == 3)
              = let [a,b,c] = list
                    value (_,(_,Byte x)) = x
                    (sec, (addr,_)) = a
                    c' = (((fromIntegral (value c) `shiftL` 16)  :: Word32) .&. 0xFF0000)
                    b' = (((fromIntegral (value b) `shiftL` 8)  :: Word32) .&. 0xFF00)
                    a' = ((fromIntegral (value a))  :: Word32)
                    word' = c' .|. b' .|. a'
                    l' =  (sec, (addr, Numeric word')):[]
                in accum ++ (l':[])
           pack list accum | (length list >= 4)
              = let (l, r) = splitAt 4 list
                    [a,b,c,d] = l
                    value (_,(_,Byte x)) = x
                    (sec, (addr,_)) = a
                    d' = (((fromIntegral (value d) `shiftL` 24) :: Word32) .&. 0x0FF000000)
                    c' = (((fromIntegral (value c) `shiftL` 16)  :: Word32) .&. 0xFF0000)
                    b' = (((fromIntegral (value b) `shiftL` 8)  :: Word32)   .&. 0xFF00)
                    a' = ((fromIntegral (value a))  :: Word32)
                    word' = d' .|. c' .|. b' .|. a'
                    l' =  (sec, (addr, Numeric word')):[]
                in pack r (accum ++ l':[])
           p = if length bytes > 0
                  then concat $ pack bytes []
                  else []
       s2 <- resolveSymbols pc cons' rest
       return (left ++ s2 ++ p)

resolveSymbols pc cons (Symbol l : rest)
  = do s <- resolveSymbols pc cons rest
       return ((l, (pc, Code)) : s)

resolveSymbols pc cons (Instruction _ : rest)
  = resolveSymbols (pc + 4) cons rest

resolveSymbols pc cons (_ : rest)
  = resolveSymbols pc cons rest


resolveSymbols'
  :: String
  -> Word32 -- program counter
  -> Int -- constants
  -> [ParseElement]
  -> IO (Int, [(String, (Word32, SectionType))])

resolveSymbols'  _ _ cons []
  = return (cons, [])

resolveSymbols' symbol pc cons (DotProp ".word" [word] : rest)
  = do let  size = fromIntegral (4) :: Int
       (cons',s) <- resolveSymbols' symbol pc (cons+size) rest
       case word of
            String n -> return (cons', ((symbol, (fromIntegral cons :: Word32, Alpha n )) : s))
            Word w -> return (cons', ((symbol, (fromIntegral cons :: Word32, Numeric w )) : s))
            Int w -> return (cons', ((symbol, (fromIntegral cons :: Word32, Numeric (fromIntegral w :: Word32) )) : s))

resolveSymbols' symbol pc cons (DotProp ".short" [word] : rest)
  = do let  size = fromIntegral (4) :: Int
       (cons',s) <- resolveSymbols' symbol pc (cons+size) rest
       case word of
            Word w -> return (cons', ((symbol, (fromIntegral cons :: Word32, Numeric w )) : s))

resolveSymbols' symbol pc cons (DotProp ".ascii" [word] : rest)
  = do let  String name = word
            size = fromIntegral (constSize word) :: Int
       (cons',s) <- resolveSymbols' symbol pc (cons+size) rest
       return (cons', ((symbol, (fromIntegral cons :: Word32, Ascii name )) : s))

resolveSymbols' symbol pc cons (DotProp ".byte" [word] : rest)
  = do let  Word byte = word
            size = fromIntegral 1 :: Int
       (cons',s) <- resolveSymbols' symbol pc (cons+size) rest
       return (cons', ((symbol, (fromIntegral cons :: Word32, Byte (fromIntegral byte :: Word8) )) : s))


resolveSymbols' symbol pc cons (DotProp ".comm" [word, Word size, align] : rest)
  = do let  String name = word
            size' = fromIntegral 4 :: Int
       (cons',s) <- resolveSymbols' symbol pc (cons + size') rest
       return (cons', ((symbol, (fromIntegral cons :: Word32, Common name )) : s))


resolveSymbols' symbol pc cons (_ : rest)
  = resolveSymbols' symbol pc cons rest

----------------------------------------------------------------------
-- Replace symbols with addresses.
----------------------------------------------------------------------
replaceSymbols
  :: [ParseElement]            -- elements being parsed
  -> Int                       -- current line number in source file
  -> Word32                    -- current address in memory
  -> [(String, (Word32, SectionType))]        -- table of labels
  -> Word32                    -- origin
  -> [(RegisterName, Word32)]  -- initial register bindings
  -> [Instruction]             -- instruction accumulator list
  -> [(Word32, Constant)]      -- constant accumulator list
  -> IO Program

----------
replaceSymbols [] line addr _ origin regBindings iAccum cAccum
  = return Program
      { memorySize = addr
      , origin = origin
      , regInit = reverse regBindings
      , instructions = reverse iAccum
      , constants = nub $ reverse cAccum
      }

----------
replaceSymbols (Instruction i : rest) line addr lTab origin regBindings iAccum cAccum
  = do
    i' <- case i of
               B   (Lab l) -> replaceBranch B   lTab addr line l
               Beq (Lab l) -> replaceBranch Beq lTab addr line l
               Bgt (Lab l) -> replaceBranch Bgt lTab addr line l
               Bl  (Lab l) -> replaceBranch Bl  lTab addr line l
               Blt (Lab l) -> replaceBranch Blt lTab addr line l
               Bne (Lab l) -> replaceBranch Bne lTab addr line l
               Bge (Lab l) -> replaceBranch Bge lTab addr line l
               Ble (Lab l) -> replaceBranch Ble lTab addr line l
               Bls (Lab l) -> replaceBranch Bls lTab addr line l
               Ldr r (LabRel l offset) -> replaceBranch' r lTab addr line l offset
               Ldr r (Lab l) -> replaceBranch'' r lTab addr line l
               _           -> return i
    replaceSymbols rest line (addr + 4) lTab origin regBindings (i' : iAccum) cAccum

----------
replaceSymbols (Newline : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols rest (line + 1) addr lTab origin regBindings iAccum cAccum


replaceSymbols ((Symbol s):(PropSection sec) : rest) line addr lTab origin regBindings iAccum cAccum
  =  do
     let declared = filter (\(s',_) -> s == s') lTab
         cons = foldl (\ accum (_,(addr,name)) ->
                            case name of
                                 Alpha a -> accum ++ (addr, Pair a (Word 0)):[]
                                 Numeric n ->  accum ++ (addr, Word n):[]
                                 Common a -> accum ++ (addr, Pair a (Word 0)):[]
                                 Pointer p -> accum ++ (addr, (Word p)):[]
                                 Ascii s -> accum ++ (addr, (String s)):[]
                                 --Byte s -> accum ++ (addr, (Word (fromIntegral s :: Word32))):[]
                      ) [] declared
     replaceSymbols (rest) line addr lTab origin regBindings iAccum (cons ++ cAccum)

----------
replaceSymbols ((PropSection ps) : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols (ps ++ rest) line addr lTab origin regBindings iAccum cAccum
----------
replaceSymbols (CodeSection l is : rest) line addr lTab origin regBindings iAccum cAccum
  = do
    replaceSymbols (l:is ++ rest) line addr lTab origin regBindings iAccum cAccum
----------

replaceSymbols ((Symbol s) : rest) line addr lTab origin regBindings iAccum cAccum
  = do
    let origin' = if s == "main"
                     then origin + fromIntegral ((length iAccum) * 4)
                     else origin

    replaceSymbols (rest) line addr lTab origin' regBindings iAccum cAccum

----------

replaceSymbols (_ : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols rest line addr lTab origin regBindings iAccum cAccum



----------------------------------------------------------------------
--
----------------------------------------------------------------------
replaceBranch branchInstruction lTab addr line label
  = let a = lookup label lTab
    in case a of
         Nothing
           -> --error ("label " ++ label ++ " not bound, line " ++ show line)
             return $
             branchInstruction (Rel 0)
         Just (addr', _)
           -> return $
             branchInstruction (Rel (fromIntegral addr' - fromIntegral addr))

replaceBranch' reg lTab addr line label offset
  = let options = filter ( \ (symbol, _) -> symbol == label ) lTab
        off = fromInteger offset
        index = off `div` 4
        (label', (addr, name)) = options !! (index)
    in if label == label'
          then do
               return $ Ldr reg (Con (fromIntegral addr))
          else error ("const label " ++ label ++ " not bound, line " ++ show line)

replaceBranch'' reg lTab addr line label
  = let a = lookup label lTab
    in case a of
         Nothing
           -> error ("const label " ++ label ++ " not bound, line " ++ show line)
         Just (addr', _)
           -> do
              return $ Ldr reg (Con (fromIntegral addr'))

----------------------------------------------------------------------
-- Assemble a program text string into a program.
----------------------------------------------------------------------
asmString
  :: String
  -> IO (Either ([(String, (Word32, SectionType))] ,Program) String )

asmString progString
  = do
    let prog = papply pProgram progString
    case prog of
         ((prog', "") : _)
           -> do lTab <- resolveSymbols 0 0 prog'
                 let common = concat $ map (getProps [] selectCommon) prog'
                 let base = 0
                 (lTab', base') <- linkCommon lTab common base
                 let commons = varNames [] common
                 sections <- getSections [] prog'
                 (_, lTab'') <- linkLocalWords lTab' base' commons sections
                 sy <- replaceSymbols prog' 1 0 lTab'' 0 [] [] []
                 return $ Left (lTab'', sy)
         ((prog', str) : _)
           -> return $ Right (errorMessage prog' str)


linkLocalWords
  :: [(String, (Word32, SectionType))]
  ->  Word32
  -> [String]
  -> [String]
  ->  IO (Word32, [(String, (Word32, SectionType))])

linkLocalWords lTab base_ commons dataSections
  = do
    let f = mapAccumL (\ accum (sec, (local, section)) ->
             case section of
                   Alpha s -> if not (elem s commons)
                                  then let base' = accum
                                       in  (accum + 4, (sec, (base', section)))
                                  else (accum, (sec, (local, section)))
                   Ascii s -> if not (elem s commons)
                                  then let base' = accum
                                           accum' = fromIntegral ((length s `div` 4 + 1) * 4)
                                       in  (accum + accum', (sec, (base', section)))
                                  else (accum, (sec, (local, section)))
                   Numeric n -> let base' = accum
                               in (accum + 4, (sec, (base', section)))
                   _ -> (accum, (sec, (local, section)))

           ) base_ lTab
        (base__, lTab') = f

        g = mapAccumL (\ accum (sec, (local, section)) ->
             case section of
                   Alpha s -> if elem (show section) dataSections
                                  then let base' = accum
                                       in case findIndices (\(sec,_) -> sec == show section) lTab' of
                                               i:is -> let (_,(start,_)) = lTab' !! i
                                                       in (accum, (sec, (local, Pointer start)))
                                               []   -> error $ show (section, lTab')
                                  else (accum, (sec, (local, section)))
                   _ -> (accum, (sec, (local, section)))

           ) base__ lTab'
    return g

linkCommon
  :: [(String, (Word32, SectionType))]
  ->  [ParseElement]
  ->  Word32
  ->  IO ([(String, (Word32, SectionType))], Word32)

linkCommon lTab [] base = return (lTab, base)

linkCommon lTab (DotProp prop elems : rest) base
  = do
    let (String var) : (Word size) : (Word align) :[] = elems
        f (symbol, (local, Alpha var')) = var == var'
        f (symbol, (local, Common var')) = var == var'
        f _ = False
        size' = if  size `mod` 4 == 0
                    then size
                    else ((size `div` 4 + 1) * 4)

    lTab' <- mapM ( \(sec, (local, section)) ->
                        if f (sec, (local, section))
                           then do
                                if var == "mem"
                                   then return (sec, (36, section))
                                   else return (sec, (base, section))
                           else return (sec, (local, section))
                  ) lTab
    linkCommon lTab' rest (base + size')

getProps
  :: [ParseElement]
  -> (ParseElement -> Bool)
  -> ParseElement
  -> [ParseElement]

getProps accum select (PropSection sec) = accum ++ filter select sec
getProps accum select (CodeSection _ elems) = concat $ map (getProps accum select) elems
getProps accum _ _ = accum

getSections accum []
  = return accum
getSections accum ((CodeSection (Symbol s) ps):ss)
  = getSections (accum ++ s:[]) ss
getSections accum (x:ss)
  = getSections accum ss

selectCommon (DotProp  ".comm" _ ) = True
selectCommon _ = False

selectLocalWords (DotProp  ".word" _ ) = True
selectLocalWords _ = False


varNames ns [] = ns
varNames ns ((DotProp _ elems):props)
  = let ns' = filter (\e -> case e of { String s -> True; _ -> False}  ) elems
        ns'' = map (\(String s) -> s) ns'
    in varNames (ns ++ ns'') props


locateData
  :: [(String, (Word32, SectionType))]        -- table of labels
  -> String
  -> Maybe Word32

locateData lTab section
  = let f (_,(_, Alpha sec)) = sec == section
        f _ = False
    in case find f lTab of
             Just (_,(addr,_)) -> Just addr
             Nothing -> Nothing

----------------------------------------------------------------------
-- Generate an error message.
----------------------------------------------------------------------
errorMessage prog' remainingInput
  = let lines = countLines prog' 1
        errLine = dropWhile isSpace (head (lines' remainingInput))
    in ("error, line " ++ show lines ++ ": " ++ errLine)
  where
    countLines [] accum
      = accum
    countLines (Newline : rest) accum
      = countLines rest (accum + 1)
    countLines (_ : rest) accum
      = countLines rest accum


lines'
  :: String
  -> [String]

lines' ""
  = []
lines' s
  = let (l,s') = break (\x -> or [x == '\n', x == '\r']) s
    in l : case s' of
             []
               -> []
             (_:s'')
               -> lines' s''


----------------------------------------------------------------------
-- Assemble a text file into a program.
----------------------------------------------------------------------
asmFile
  :: String
  -> IO (Either ([(String, (Word32, SectionType))] ,Program) String )

asmFile fileName
  = do file <- readFile fileName
       asmString file



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
{-
p1 = "            origin 16\n" ++
     "            reg r0 = DATA1\n" ++
     "\n" ++
     "TOP:        mov r1, #100		; this is the top of the loop\n" ++
     "LOOP:       add r1, r1, #4\n" ++
     "            cmp r1, #200\n" ++
     "            bne LOOP\n" ++
     "            swi #11\n" ++
     "\n" ++
     "DATA1     = 0,1,2\n" ++
     "            3,4,5\n" ++
     "\n" ++
     "DATA2     = 100\n" ++
     "\n" ++
     "MSG1      = \"Hello, World!\"\n"


p2 =
    ";---------------------------------------------------------------------\n" ++
    ";- FILE:              p1.arm\n" ++
    ";- DESCRIPTION:       \n" ++
    ";- DATE:              04/04/2001\n" ++
    ";- PROJECT:           \n" ++
    ";- LANGUAGE PLATFORM: VARM (Virtual ARM), for CSE240 Spring 2001\n" ++
    ";- OS PLATFORM:       RedHat Linux 6.2\n" ++
    ";- AUTHOR:            Jeffrey A. Meunier\n" ++
    ";- EMAIL:             jeffm@cse.uconn.edu\n" ++
    ";---------------------------------------------------------------------\n" ++
    "\n" ++
    "            origin 0\n" ++
    "            reg r0 = MSG\n" ++
    "            reg r9 = BUFFER\n" ++
    "\n" ++
    "            swi #2\n" ++
    "            mov r0, r9\n" ++
    "            mov r1, #32\n" ++
    "            swi #4\n" ++
    "\n" ++
    "            swi #11\n" ++
    "\n" ++
    "MSG       = \"Enter your name: \"\n" ++
    "BUFFER    = array 32 0\n"
-}


----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
