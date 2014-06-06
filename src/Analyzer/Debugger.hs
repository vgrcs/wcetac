-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Debugger
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
{-# LANGUAGE NamedFieldPuns  #-}
module Analyzer.Debugger ( extractPC

) where

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------

import Arm.RegisterName
import Arm.Register
import Arm.Format
import Arm.Pipeline
import Arm.CPU
import Analyzer.LRU
import Analyzer.Label
import Analyzer.Certificate
import Arm.Memory
import Arm.Decoder

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Array
import Data.Vec
import Data.Word
import System.IO.Unsafe
import Control.Monad.Par
import Control.Monad.Par.Internal hiding (Done)



data Entry = Log {cmp :: Component, level:: Level, msg:: String} deriving (Eq, Show)
data Level  =  Everything
            |  Entry_Exit
            |  Extra
            deriving (Show, Eq)

data Component = ProcessorE | ExecutionUnitE | MemoryE | PipelineE deriving (Eq, Show)
data Components = MainMemory | CacheMemory | RegistersFile | PipelineWindow | TypeSystem deriving (Eq, Show)

pipelineFromMainCPU cpu
  = pipeline $ (multi cpu) Map.! 0

registersFromMainCPU cpu
  = registers $ (multi cpu) Map.! 0

showCPU
  :: (Show a) => [Components]
  -> (CPU a)
  -> String
  -> String

showCPU cmp cpu str'
  = let line = "\n+" ++ (List.take 74 (repeat '-')) ++ "+" ++ "\n"
        str1 = case elem PipelineWindow cmp of
                         True -> let p = pipelineFromMainCPU cpu  -- pipeline (core cpu)
                                 in str' ++ ("|" ++ (formatStr 74 ' ' "Pipeline") ++ "|")
                                         ++ (show p)
                         _ -> str'
        str2 = case elem RegistersFile cmp of
                         True -> str1 ++ ("|" ++ (formatStr 74 ' ' "Registers") ++ "|") ++ line
                                     -- ++ show (registers (core cpu)) ++ line
                                     ++ show (registersFromMainCPU cpu) ++ line
                         _ -> str1
        str4 = case elem MainMemory cmp of
                         True -> str2 ++ ("|" ++ (formatStr 74 ' ' "Cache") ++ "|") ++  line
                                      ++ show (memory cpu) ++ line
                         _ -> str2
    in str4


instance Show Registers where
  show regs
    = let str =  (formatS 74 '=' " Registers =") ++ "\n"
          showReg regName
             = let regVal = getReg regs regName
               in ((show regName) ++ "=" ++ (formatN regVal))
          str' =  str ++ "  " ++ showReg R0 ++ "  " ++ showReg R4 ++ "   " ++ showReg R8 ++ "  " ++ showReg R12 ++ "\n"
                      ++ "  " ++ showReg R1 ++ "  " ++ showReg R5 ++ "   " ++ showReg R9 ++ "  " ++ showReg R13 ++ "\n"
                      ++ "  " ++ showReg R2 ++ "  " ++ showReg R6 ++ "  " ++ showReg R10 ++ "  " ++ showReg R14 ++ "\n"
                      ++ "  " ++ showReg R3 ++ "  " ++ showReg R7 ++ "  " ++ showReg R11 ++ "  " ++ showReg R15 ++ "\n"
                      ++ showCPSRFlags' regs
      in str'

showCPSRFlags' regs
  = let  n =  cpsrGetN regs
         z =  cpsrGetZ regs
         c =  cpsrGetC regs
         v =  cpsrGetV regs
    in ("N=" ++ show n ++ " Z=" ++ show z ++ " C=" ++ show c ++ " V=" ++ show v)


instance Show SharedMemory where
  show m = let title = formatS 74 '=' " Data Cache ="
               DataMem arr = datam m
               list = assocs arr
               loop (l:ls) str
                = let (addr, v) = l
                      str' = case v of
                             MemValue val -> str ++ " " ++ (formatN (StdReg (addr * 4) )) ++ ": " ++ show (Interval val) ++ "; " ++ "\n"
                             BottomI -> str ++ " " ++  ": " ++ "_|_" ++ "\n"
                             StdMem val -> str ++ " " ++ (formatN (StdReg (addr * 4))) ++ ": " ++ show val ++ "; " ++ "\n"
                  in loop ls str'
               loop [] str = str ++ "\n"
           in loop list ("+" ++ title ++ "+\n" )

instance Show Memory where

  showsPrec s m
     = let  main = must m
            addrs = [0, 4 .. 100]
            instr = List.map (\pc -> let (cl, opcode, _) = unsafePerformIO $ readMemInstrWord m pc
                                    in case opcode of
                                            OpCode op -> let Just i = Arm.Decoder.decode op
                                                        in "[" ++ show pc ++ "] " ++ show i
                                            BottomW  -> "[" ++ show pc ++ "] " ++ "Nothing"
                             ) addrs
       --in error ((unlines instr))
       in ((unlines instr) ++ )


formatS places char str
  = let pad = places - (List.length str)
        fill = (List.take (pad `div` 2) (repeat  char))
        str' = fill ++ str ++ fill
    in if List.length str' `mod` 2 == 0 then str' else str'  ++ [' ']


formatN n
  = let s = show n
        pad = 5 - (List.length s)
    in (List.take pad (repeat ' ')) ++ s

formatL n
  = let s = show n
        pad = 5 - (List.length s)
    in " " ++ s ++ (List.take pad (repeat ' '))



data Markers = Top | Header String String | Columns |
               LineSep | TSep | KSep | StageSep | ISep |
               PCSep | BlockedSep | StubSep

instance Show Markers where
  show Top = "+" ++ (List.take 96 (repeat '-')) ++ "+" ++ "\n"
  show (Header pc cpsr)
    =  ("|PState: " ++ (formatStr 88 ' ' ("next fetch=" ++ pc ++ "; blocked= " ++ cpsr)) ++ "|" ++ "\n")
  show Columns = show LineSep ++  ("| n   | k | Next stage |" ++   (formatStr 40 ' ' "State") ++
                                   "| Pc |          Blocked          |" ++ "\n") ++ show LineSep
  show TSep = List.take 5 (repeat '-') ++ "+"
  show KSep = List.take 3 (repeat  '-') ++ "+"
  show StageSep = List.take 12 (repeat  '-') ++ "+"
  show ISep = List.take 40 (repeat  '-') ++ "+"
  show PCSep = List.take 4 (repeat  '-') ++ "+"
  show LineSep = "+" ++ show TSep ++ show KSep ++ show StageSep ++ show ISep ++ show PCSep ++ show BlockedSep ++ "\n"
  show BlockedSep = List.take 27 (repeat  '-') ++ "+"
  show StubSep = "|" ++ show TSep ++ "|" ++ show KSep ++ "|" ++ show StageSep ++ "|"



instance (Show a) => Show (PState a) where
  show s@PState { simtime = t, nextpc, cpsr, regfile = file, mem, shared = sm, coords = Coord vec }
     = let v = toList vec
           pc = List.map extractPC2 (toList vec)
           b = List.map extractBlocked (toList vec)
           m = List.map extractMem2 (toList vec)
           r = List.map extractFile2 (toList vec)
           [arg0, arg1, arg2] = List.zip5 v pc b r m
           context = show Top ++ show (Header (show nextpc) (printCPSR cpsr))
           line k ( at@AbsTaskState {property = x, stage = s, task = c }, pc, b, r, m)
               =  showsPrec k at  "" ++
                  (formatStr 4 ' ' (show pc)) ++ ("|") ++
                  (formatStr 26 ' ' b)  ++ (" |") ++ "\n" ++  (show LineSep) ++
                  (showStubs (stubsFromTask c)) ++ "\n" ++  (show LineSep)
       in context ++ (show Columns) ++ line 0 arg0 ++ line 1 arg1 ++ line 2 arg2


instance (Show a) => Show (AbsTaskState a) where
   showsPrec k at@AbsTaskState { property = x, stage = s, task = c }
     = ((("|" ++ (formatStr 4 ' ' (show x)) ++ " | " ++ show k ++ " |" ++
                        (formatStr 12 ' ' (show s)) ++ "|") ++
                        (formatStr 40 ' ' (show c)) ++ "|" ) ++)


showStubs l
   =  let f accum (n, StdReg c) =  accum  ++ ", cpsr=" ++  (showCPSR c)
          f accum (n, other) = accum ++ show n ++ "= "  ++ show other ++ ","
      in "|" ++ "stubs: " ++ (formatStr3 89 ' ' (List.foldl  f "" (Map.toList l))) ++ "|"


showRegistesAsList l
   = List.foldl (\accum (n,v)  -> case v of
                                      StdReg c -> accum  ++ (showCPSR c)
                                      other -> accum ++ show (n,v)
                ) "" l

showCPSR :: Word32 -> String
showCPSR w
  = let  n =  getControlN w
         z =  getControlZ w
         c =  getControlC w
         v =  getControlV w
    in ("N=" ++ show n ++ " Z=" ++ show z ++ " C=" ++ show c ++ " V=" ++ show v)

printPC :: Registers -> String
printPC regs
    = let StdReg pc = getReg regs R15
      in show pc

printCPSR :: Word32 -> String
printCPSR cpsr
   = let  regs = listArray (R0, CPSR) (repeat BottomR)
          (names,_) = unzip (assocs regs)
          l = List.map (blocked' cpsr) names
          l'= List.filter (\(r,f) -> f == True) l
          flags = List.foldl (\t (r,_) -> show r ++ "; " ++ t) "" l'
     in flags


extractReason (Stalled r _ _) = r

extractFile2 at@AbsTaskState { task = state }
  = let Registers regs = extractFile state
        maps = assocs regs
    in [ (i,v) | (i,v) <- maps, v /= BottomR]

extractMem2 at@AbsTaskState { task = state }
  = extractMem state

extractPC2 at@AbsTaskState { task = state }
  = extractPC state

extractPC state
   = let stub = extractStub state
         pc = case  state of
                             Done _ -> getReg (extractFile state) R15
                             _ -> let s = extractStub state
                                  in getRegStub s R15
     in case pc of
        StdReg pc_ ->  pc_
        BottomR -> 0
        other -> error ("X" ++ show (getReg (extractFile state) R15))

extractBlocked at@AbsTaskState { task = state }
  = printCPSR (extractCpsr state)

blocked'
  :: Word32
   -> RegisterName
   -> (RegisterName, Bool)

blocked' cpsr reg
   = if fromIntegral (cpsrGetByName reg cpsr) == 1 then (reg, True) else (reg, False)


extractStub (Ready _) = Map.empty
extractStub (Fetched _ stub) = stub
extractStub (Decoded _ stub)  = stub
extractStub (Stalled _ _ stub) = stub
extractStub (Executed _ stub) = stub

extractFile (Done t@Task { taskRegisters = file } ) = file
extractFile (Ready t@Task { taskRegisters = file }) = file
extractFile (Fetched t@Task { taskRegisters = file } _) = file
extractFile (Decoded t@Task { taskRegisters = file } _) = file
extractFile (Stalled _ t@Task { taskRegisters = file }_) = file
extractFile (Executed t@Task { taskRegisters = file }_) = file

extractCpsr (Ready t@Task { taskCpsr = cpsr } ) = cpsr
extractCpsr (Fetched t@Task { taskCpsr = cpsr } _) = cpsr
extractCpsr (Decoded t@Task { taskCpsr = cpsr } _) = cpsr
extractCpsr (Done t@Task { taskCpsr = cpsr }) = cpsr
extractCpsr (Stalled _ t@Task { taskCpsr = cpsr } _) = cpsr
extractCpsr (Executed t@Task { taskCpsr = cpsr } _) = cpsr

extractState (Ready _)        = "Ready"
extractState (Fetched _ _)    = "Fetched"
extractState (Decoded _ _)    = "Decoded"
extractState (Done _)         = "Done"
extractState (Stalled _ _ _)  = "Stalled"
extractState (Executed _ _)   = "Executed"

extractMem (Ready t@Task { taskMemory = m } ) = m
extractMem (Fetched t@Task { taskMemory = m } _) = m
extractMem (Decoded t@Task { taskMemory = m } _) = m
extractMem (Done t@Task { taskMemory = m } ) = m
extractMem (Stalled _ t@Task { taskMemory = m } _)  = m
extractMem (Executed t@Task { taskMemory = m } _) = m

extractFileFromAbsState at@AbsTaskState { task = t } = extractFile t


showFrame
    :: (Show a) => [Components]
    -> Invs (CPU a)
    -> [Integer]
    -> String
    -> String

showFrame _ frame [] str
    =  str
showFrame cmp frame (i:is) str
    = let Just (n@Node { var = cpu } ) = Map.lookup i frame
          Just cpu' = unsafePerformIO $ pollIVar cpu
          str' = showCPU cmp cpu' ""
      in str ++ showFrame cmp frame is str'

showLoopInfo frame [] str
    =  str

showLoopInfo node (i:is) str
    = let str' = showsPrec i node
      in str ++ showLoopInfo node is (str' "")

showCert
    :: (Show a) => [Components]
    -> Invs (CPU a)
    -> String
showCert cmp frame
   =  showFrame cmp frame (Map.keys frame) ""

printCert cmp frame = putStrLn (showCert cmp frame)



invsFixs invariants
   = let f (n,(_,_,fix)) = (n,fix)
     in putStrLn (show (List.map f invariants))

invsCPUs invariants
   = let f (k, n@Node { var = v })
             = let id = case unsafePerformIO $ pollIVar v of
                              Just cpu -> active cpu
                              Nothing -> (-1,-1)
               in show (k, id)
     in mapM putStr (List.map f (Map.toList invariants)) >> putStrLn ""


showNodeLoops :: NodeCount -> IO ()
showNodeLoops l
   = let f (n, c) = ("Node " ++ show n ++ " => " ++ show c)
     in mapM_ putStrLn (List.map f (Map.toList l))

showEdgeLoops :: EdgeCount -> IO ()
showEdgeLoops l
   = let f (n, c) = ("Edge " ++ show n ++ " => " ++ show c)
     in mapM_ putStrLn (List.map f (Map.toList l))
