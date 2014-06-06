-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ScreenPrinter
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
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE CPP #-}
module Analyzer.ScreenPrinter ( extractPc, dbg, dbgStep, dbgState, getPC, extractInstr, extractStub,
                                extractCpsr, extractMem

) where

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import Data.Vec
import Data.Array
import Data.Word
import Data.Maybe
import System.IO.Unsafe
import Data.Char
import Data.Bits

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Pipeline
import Arm.Format
import Arm.Register
import Arm.Memory
import Arm.RegisterName
import Arm.Instruction
import Arm.Decoder
import Arm.CPU
import Analyzer.LRU
import Analyzer.Label hiding (shift)
import Analyzer.Certificate
import Analyzer.ValueAbstraction


#if defined(SIM)
--pointsDebug = [58,59,60,61,62]
pointsDebug = []
--pointsDebug = [30,33,47,53]
thiSide = Supplier -- Consumer
#else
pointsDebug = []
thiSide = Supplier
#endif


dbgPriorities :: Vec3 Bool -> Label -> IO ()
dbgPriorities vec label
  = if elem (ppoint label) pointsDebug
       then putStrLn $ show (toList vec)
       else return ()

dbgState msg p@PState { regfile, mem, shared } label
  = if elem (ppoint label) pointsDebug
       then do
            putStrLn msg
            cmd <- getLine
            putStrLn ""
            case cmd of
                 'p':[] -> putStrLn $ show $ p
                 'm':[] -> do putStrLn $ show shared
                 'q':[] -> return ()
                 '1':[] -> putStrLn $ showsPrec 3 mem ""
                 _ -> dbgState msg p label
       else return ()

dbgStep msg p label side
  = if elem (ppoint label) pointsDebug && side == thiSide
       then do
            putStrLn msg
            let loop = do
                       cmd <- getLine
                       putStrLn ""
                       let s@PState { regfile, mem, shared } = p
                       case cmd of
                            'p':[] -> do putStrLn $ show p
                                         loop
                            'r':[] -> do putStrLn $ show regfile
                                         loop
                            'm':[] -> do putStrLn $ show shared
                                         loop
                            '3':[] -> do putStrLn $ showsPrec 3 mem ""
                                         loop
                            '2':[] -> do putStrLn $ showsPrec 2 mem ""
                                         loop
                            '1':[] -> do putStrLn $ showsPrec 1 mem ""
                                         loop
                            'q':[] -> return ()
            loop
      else return ()

dbg msg cpu@CPU{ memory, multi, active = (parent, child) } label side
  =  if elem (ppoint label) pointsDebug && side == thiSide
        then do
             putStrLn msg
             let loop = do
                        let core = multi Map.! child
                        cmd <- getLine
                        putStrLn ""
                        case cmd of
                             'a':[] -> do putStrLn $ show (parent,child)
                                          loop
                             'm':[] -> do putStrLn $ show (memory)
                                          loop
                             'r':[] -> do putStrLn $ show (registers core)
                                          loop
                             'p':[] ->  do putStrLn $ show ((pipeline core))
                                           loop
                             '3':[] -> do putStrLn $ showsPrec 3 (instrMem core) ""
                                          loop
                             '2':[] -> do putStrLn $ showsPrec 2 (instrMem core) ""
                                          loop
                             '1':[] -> do putStrLn $ showsPrec 1 (instrMem core) ""
                                          loop
                             'q':[] -> return ()
                             _ -> return ()

             loop
        else return ()

instance Show Registers where
  show regs
    = let str =  (formatS 74 '=' " Registers =") ++ "\n"
          control = getReg regs CPSR
          showReg regName
             = let regVal = getReg regs regName
               in formatR $ (show regName) ++ "=" ++ (show regVal)
          str' =  str ++ "  " ++ showReg R0 ++ "  " ++ showReg R4 ++ "  " ++ showReg R8 ++ "  " ++ showReg R12 ++ "\n"
                      ++ "  " ++ showReg R1 ++ "  " ++ showReg R5 ++ "  " ++ showReg R9 ++ "  " ++ showReg R13 ++ "\n"
                      ++ "  " ++ showReg R2 ++ "  " ++ showReg R6 ++ "  " ++ showReg R10 ++ "  " ++ showReg R14 ++ "\n"
                      ++ "  " ++ showReg R3 ++ "  " ++ showReg R7 ++ "  " ++ showReg R11 ++ "  " ++ showReg R15 ++ "\n"
                      ++ show control
      in str'




formatS places char str
  = let pad = places - (List.length str)
        fill = (List.take (pad `div` 2) (repeat  char))
        str' = fill ++ str ++ fill
    in if List.length str' `mod` 2 == 0 then str' else str'  ++ [' ']

formatN n
  = let s = show n
        pad = 5 - (List.length s)
    in (List.take pad (repeat ' ')) ++ s

formatR r
  = let pad = 20 - (List.length r)
    in r ++ (List.take pad (repeat ' '))

instance (Show a) => Show (Node a) where
  --show n = "fixpoint= " ++ show (stableFixpoint n) ++
  --         ", value= " ++ show (stableValue n) ++
  --         ", loop= " ++ show (insideLoop n)
  --show n = "node= " ++ show (redirect n)
  show n = "node= " ++ show (value n)



instance (Show a) => Show (InvsWrapper a) where
  show (InvsWrapper invs)
    = let l = Map.toList invs
          f accum (p, n) = accum ++ show p ++ " -> " ++ show n ++ "\n"
      in List.foldl f "" l

instance Show SharedMemory where
  show m
    = let DataMem arr = datam m
          g True = "x"
          g False = " "
          f accum (addr, v)
            =  case v of
                    RegVal val -> accum ++  (formatN (addr * 4)) ++ ": " ++ show (RegVal val) ++ "; " ++ "\n"
                    BackVal val -> accum ++  (formatN (addr * 4)) ++ ": " ++ show (BackVal val) ++ "; " ++ "\n"
                    PtrVal val -> accum ++ (formatN (addr * 4)) ++ ": " ++ show (PtrVal val) ++ "; " ++ "\n"
                    PcVal val -> accum ++   (formatN (addr * 4)) ++ ": " ++ show (PcVal val) ++ "; " ++ "\n"
                    LrVal val -> accum ++  (formatN (addr * 4)) ++ ": " ++ show (LrVal val) ++ "; " ++ "\n"
                    ConVal val -> accum ++  (formatN (addr * 4)) ++ ": " ++ show (ConVal val) ++ "; " ++ "\n"
                    CharVal val -> accum ++  (formatN (addr * 4)) ++ ": "  ++ fetchString m (addr * 4) ++ "\n"
                    StdVal val -> accum ++  (formatN (addr * 4)) ++ ": "  ++ show (StdVal val) ++ "\n"

      in case assocs arr of
              [] -> "\n"
              l -> let l' = filter (\(a,b) -> b /= Bottom) l
                  in List.foldl f "" l'



instance Show Memory where
  showsPrec 2 m
     = let  L2Box l2 = mustL2 m
       in ((show (CacheWrapper l2)) ++ )
  showsPrec 1 m
     = let  L1Box l1 = mustL1 m
       in ((show (CacheWrapper l1)) ++ )

  showsPrec 3 m
     = let  main = must m
            arr = filter (\(a,v) -> v /= BottomW ) (assocs main)
            instr = List.map (\(a,v) -> case v of
                                            OpCode op -> let i = Arm.Decoder.decode op
                                                        in "[" ++ show a ++ "] " ++ show (fromJust i)
                                            BottomW  -> "[" ++ show a ++ "] " ++ "Nothing"
                             ) arr
       --in error ((unlines instr))
       in ((unlines instr) ++ )
  showsPrec 0 m = showsPrec 1 m





data Markers = Top | Header String String String String String String String String String String
               | Columns |
               LineSep | TSep | KSep | BSep | StageSep | ISep |
               PCSep | BlockedSep | StubSep

instance Show Markers where
  show Top = "+" ++ (List.take 102 (repeat '-')) ++ "+" ++ "\n"
  show (Header simtime pc cpsr regfile targets mem shared periods stable b)
    =  ("|PState: " ++ (formatStr 93   ' ' ("simtime=" ++ (formatStr 5 ' ' simtime) ++
                                         "; next fetch=" ++ pc ++
                                         "; blocked= " ++ cpsr ++ "; periods= " ++ periods ++
                                         "; stable= " ++ stable ++ "; busy= " ++ b))
                    ++ "|" ++ "\n" ++
        show Top ++ "Regfile: " ++ regfile ++ "\n"   ++
                   -- "Shared: " ++ shared ++ "\n"  ++
                   "Instrs: " ++ targets ++ "\n" ) --  ++
                   -- "Cache: " ++ mem ++ "\n")
  show Columns = show LineSep ++  ("| k | cycles |  bus  | stage |" ++   (formatStr 40 ' ' "State") ++
                                   "| Pc |          Blocked          |" ++ "\n") ++ show LineSep
  show TSep = List.take 8 (repeat '-') ++ "+"
  show KSep = List.take 3 (repeat  '-') ++ "+"
  show BSep = List.take 7 (repeat  '-') ++ "+"
  show StageSep = List.take 7 (repeat  '-') ++ "+"
  show ISep = List.take 40 (repeat  '-') ++ "+"
  show PCSep = List.take 4 (repeat  '-') ++ "+"
  show LineSep = "+" ++ show KSep ++ show TSep ++ show BSep ++ show StageSep ++ show ISep ++
                       show PCSep ++ show BlockedSep ++ "\n"
  show BlockedSep = List.take 27 (repeat  '-') ++ "+"
  show StubSep = "|" ++ show TSep ++ "|" ++ show KSep ++ "|" ++ show StageSep ++ "|"


instance (Show a) => Show (PState a) where
  show p@PState { simtime, nextpc, cpsr, regfile = Registers regs , mem, shared,
                  coords = Coord vec, targets, final, stableP, busytime, busyPeriods }
     = let v = toList vec
           pc = List.map (getPC targets) (toList vec)
           b = List.map getBlocked (toList vec)
           m = List.map getMem (toList vec)
           r = List.map getFile (toList vec)
           [arg0, arg1, arg2] = List.zip5 v pc b r m
           context = show Top ++
                     show (Header (show simtime) (show nextpc) (printCPSR cpsr)
                                  (showRegistesAsList (assocs regs))
                                  (show (TargetsWraper targets))
                                  (showsPrec 1 mem "")
                                  (show (SharedWrapper shared)) (show busyPeriods)
                                  (if stableP then "T" else "F")
                                  (show busytime))
           line k (t@AbsTaskState { task, stage }, pc, b, r, m)
               = let  stub = case stage of
                                  WB -> let Registers file = extractFile task
                                       in showRegistesAsList (assocs file)
                                  _ -> show (extractStub task)
                 in
                 showsPrec k t  "" ++
                  (formatStr 4 ' ' (show pc)) ++ ("|") ++
                  (formatStr 26 ' ' b)  ++ (" |") ++ "\n" ++ (show LineSep) ++
                  --"Task NextPC: " ++ (show (extractPc  task)) ++ "\n" ++
                  "Stubs: " ++ stub ++ "\n" ++
                  -- "Mem: " ++ (show (extractShared task)) ++ "\n" ++
                  --"Cache: " ++ m ++ "\n" ++
                  (show LineSep)
       in context ++ (show Columns) ++ line 0 arg0  ++ line 1 arg1 ++ line 2 arg2


data StubsWrapper = StubsWrapper Stubs deriving (Eq)

instance Show StubsWrapper where
  show (StubsWrapper s) =
    let l = Map.toList s
    in showRegistesAsList l


data TargetsWraper = TargetsWraper Targets

instance Show TargetsWraper where
  show (TargetsWraper s) =
    showTargetsAsList s

data SharedWrapper = SharedWrapper SharedMemory

instance Show SharedWrapper where
  show (SharedWrapper sm) =
    let  DataMem m = datam sm
         l = assocs m
    in showSharedAsList l

instance (Show a) => Show (AbsTaskState a) where
   showsPrec k t@AbsTaskState { property=x, stage=s, task=c}
     = ((("| " ++ show k ++ " | " ++  (formatStr 6 ' ' (show x)) ++ " | " ++
                        (formatStr 5 ' ' (showsPrec 0 x "")) ++ "| " ++
                        (formatStr 6 ' ' (show s)) ++ "|") ++
                        (formatStr 40 ' ' (show c)) ++ "|" ) ++)



instance Show (TaskState) where
  show (Ready task )  =  ("Ready: " ++ show (taskInstr task ))
  show (Fetched task stub) =  ("Fetched: " ++ show (taskInstr task ))
  show (Decoded task stub)  =  ("Decoded: " ++ show (taskInstr task ))
  show (Stalled r task stub)=  ("Stalled: " ++ show (taskInstr task ))
  show (Executed task stub) =  ("Executed: " ++ show (taskInstr task ))
  show (Done task)          =  ("Done: " ++ show (taskInstr task ))


getPC targets a@AbsTaskState { task }
   = --case List.lookup (extractInstr task) targets of
     --     Just pc -> pc
     --     Nothing -> extractPc task
     extractPc task

getBlocked a@AbsTaskState { task }
  = printCPSR (extractCpsr task)

getFile a@AbsTaskState { task }
  = let Registers regs = extractFile task
        maps = assocs regs
    in [ (i,v) | (i,v) <- maps, v /= Bottom]

getMem a@AbsTaskState { task }
  = show $ extractMem task


extractPc task
  = let stub = extractStub task
        pc = case  task of
                   --Done _ -> getReg (extractFile task) R15
                   Done _ -> StdVal $ extractNextPc task
                   _ -> let StubsWrapper s = extractStub task
                       in if Map.member R15 s
                             then getRegStub (s) R15
                             else Bottom
    in case pc of
       --PcVal p ->  p
       StdVal p -> p
       Bottom -> 0

extractStub (Ready _) = StubsWrapper Map.empty
extractStub (Fetched _ stub) = StubsWrapper stub
extractStub (Decoded _ stub) = StubsWrapper stub
extractStub (Stalled _ _ stub)  = StubsWrapper stub
extractStub (Executed _ stub) = StubsWrapper stub
extractStub (Done _) = StubsWrapper Map.empty

extractNextPc (Ready task)  = taskNextPc task
extractNextPc (Fetched task _) = taskNextPc task
extractNextPc (Decoded task _) = taskNextPc task
extractNextPc (Done task) = taskNextPc task
extractNextPc (Stalled _ task _) = taskNextPc task
extractNextPc (Executed task _) = taskNextPc task

extractCpsr (Ready task)  = taskCpsr task
extractCpsr (Fetched task _) = taskCpsr task
extractCpsr (Decoded task _) = taskCpsr task
extractCpsr (Done task) = taskCpsr task
extractCpsr (Stalled _ task _) = taskCpsr task
extractCpsr (Executed task _) = taskCpsr task

extractMem (Ready task)  = taskMemory task
extractMem (Fetched task _) = taskMemory task
extractMem (Decoded task _) = taskMemory task
extractMem (Done task) = taskMemory task
extractMem (Stalled _ task _) = taskMemory task
extractMem (Executed task _) = taskMemory task

extractInstr (Ready task)  = taskInstr task
extractInstr (Fetched task _) = taskInstr task
extractInstr (Decoded task _) = taskInstr task
extractInstr (Done task) = taskInstr task
extractInstr (Stalled _ task _) = taskInstr task
extractInstr (Executed task _) = taskInstr task

extractShared (Ready task)  = SharedWrapper $ taskShared task
extractShared (Fetched task _) = SharedWrapper $ taskShared task
extractShared (Decoded task _) = SharedWrapper $ taskShared task
extractShared (Done task) = SharedWrapper $  taskShared task
extractShared (Stalled _ task _) = SharedWrapper $ taskShared task
extractShared (Executed task _) = SharedWrapper $ taskShared task

extractFile (Ready task)  = taskRegisters task
extractFile (Fetched task _) = taskRegisters task
extractFile (Decoded task _) = taskRegisters task
extractFile (Done task) = taskRegisters task
extractFile (Stalled _ task _) = taskRegisters task
extractFile (Executed task _) = taskRegisters task




printCPSR :: Word32 -> String
printCPSR cpsr
   = let  regs = listArray (R0, CPSR) (repeat Bottom)
          (names,_) = unzip (assocs regs)
          blocked r = if fromIntegral (cpsrGetByName r cpsr) == 1 then (r, True) else (r, False)
          l = List.map blocked names
          l'= List.filter (\(r,f) -> f == True) l
          flags = List.foldl (\t (r,_) -> show r ++ "; " ++ t) "" l'
     in flags

showRegistesAsList l
   = let f accum (n, v) = case n of
                               CPSR -> case v of
                                           Bottom -> accum
                                           other -> accum  ++ ("cpsr=" ++ show v) ++ "; "
                               other -> case v of
                                            Bottom -> accum
                                            other -> accum ++ show n ++ "=" ++ show v ++ "; "
     in List.foldl f "" l

showSharedAsList l
  = let g True = "x"
        g False = " "
        f accum (n, v)
            = case v of
                   Bottom -> accum
                   StdVal _ -> accum
                   RegVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                        "<<" ++ show (RegVal val) ++ ">>" ++ "; "
                   BackVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                        "<<" ++ show (BackVal val) ++ ">>" ++ "; "
                   PtrVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                        "<<" ++ show (PtrVal val) ++ ">>" ++ "; "
                   PcVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (PcVal val) ++ ">>" ++ "; "
                   LrVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (LrVal val) ++ ">>" ++ "; "
                   SpVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (SpVal val) ++ ">>" ++ "; "
                   ConVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (ConVal val) ++ ">>" ++ "; "
                   MemVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (MemVal val) ++ ">>" ++ "; "
                   CharVal val -> accum ++ "0X" ++ (show (n * 4) ) ++ ": " ++
                                       "<<" ++ show (CharVal val) ++ ">>" ++ "; "
     in List.foldl f "" l

showTargetsAsList l
   = let f accum (i, pc) = accum ++ show i ++ " -> " ++ show pc ++ "; "
     in List.foldl f "" l



----------------------------------------------------------------------
-- Fetch a string from memory.
----------------------------------------------------------------------
fetchString
  :: SharedMemory
  -> Address
  -> String

fetchString mem addr
  = do let (val, mem') = unsafePerformIO $ readMemWord mem addr
       let word = case val of
                       --StdVal w -> w
                       --Bottom -> 0
                       CharVal w -> w
                       other -> error $ "fetch string error " ++ show other
       let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
       let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
       let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
       let c1 = fromIntegral (word .&. 0xFF)
       if c1 == 0
         then ""
         else if c2 == 0
                then  [chr c1]
                else if c3 == 0
                       then  [chr c1, chr c2]
                       else if c4 == 0
                              then  [chr c1, chr c2, chr c3]
                              else let s = fetchString mem (addr + 4)
                                   in ([chr c1, chr c2, chr c3, chr c4]) --  ++ s)
