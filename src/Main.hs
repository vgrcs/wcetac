{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :  prototype
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------

import Data.List
import Data.Maybe
import Data.Word
import Data.Bits
import System.Console.CmdArgs
import Control.Monad
import Data.IORef

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Instruction
import Arm.Pipeline
import Arm.Format
import Arm.BinaryNumber
import Analyzer.Semantics
import Analyzer.Label hiding (shift)
import Analyzer.PipelineModel
import Analyzer.ARM5StagePipeline
import Analyzer.Interpreter
import Analyzer.Analysis
import Analyzer.ACC
import Analyzer.Channel
import Analyzer.Analysis
import Analyzer.ValueAbstraction

data WCETAC = WCETAC { file :: FilePath,
                       graph :: FilePath,
                       datadir :: FilePath,
                       tdm :: Bool,
                       lr :: Bool,
                       analyze :: Bool,
                       check :: Bool,
                       noreduce :: Bool }
              deriving (Data,Typeable,Show,Eq)


wcetav = WCETAC {
         graph = "dependency.graphm" &= opt "dependency.graphml" &= typFile &= help "export graph to <file>",
         file = def &= args &= typFile,
         datadir = "../share/wcetac-1.0.0/" &= opt "../share/wcetac-1.0.0/" &= typDir &= help "benchmarks location <datadir>/",
         tdm = def  &= help "use the TDMA composable arbiter",
         lr = True &= help "use the LR-rate server abstraction",
         analyze = True  &= help "run the WCET analyzer",
         noreduce = False &= help "don't apply the sequential recursive transformations",
         check = False  &= help "run the WCET checker" } &=
         verbosity &=
         help "WCET Static Analyzer/Checker" &=
         summary "WCETAC v1.0.0, Vitor Rodrigues" &=
         details ["To analyze the WCET of an assembler <file> (without extension) use [FILE]",
                  "To check the WCET estimate use --check or -c",
                  "To use the TDM arbiter instead of the default LR-server use --tdma or -t",
                  "If the 'wcetac' is inside PATH, the default location of data '~/.cabal/share/wcetac-1.0.0/' can be changed using --datadir or -d",
                  "",
                  "Example of how to 'analyze' and 'check' the WCET of the assembler file 'benchmark.s': " ++
                  "'cd ~/.cabal/bin'" ++ " and then invoke " ++ "'./wcetac --check benchmark'" ]


main = do
       line <- cmdArgs wcetav
       when (file line == "") (error $ "missing benchmark file (inside default location \"~/.cabal/share/wcetac-1.0.0/\")\n" ++
                                       "use './cabal/bin/wcetac <benchmark file>' or 'wcetac <benchmark file> -d <datadir>' " )
       let path = (datadir line) ++ (file line)
       putStrLn $ "\nREADING ASSEMBLER " ++ path ++ ".s\n"
       verbosity <- isLoud

       when (tdm line) (writeIORef lrServerOff True)
       when (not (tdm line)) (putStrLn "USING THE LR-SERVER ABSTRACTION")

       let rmode = if (noreduce line) then NotReduced else Reduced

       (str, cert) <- timeStamp verbosity "GENERATION TIME SUPPLIER=" $
                      sender path rmode verbosity

       when (check line) $
                   do
                   when verbosity (putStrLn "\nSENDING CERTIFICATE...\n")
                   g <- newChannel
                   sendCert g (str, cert)
                   (str', cert') <- liftM fromJust (getCert g)
                   valid <- timeStamp verbosity "VERIFICATION TIME RECEIVER=" (receiver str' cert' rmode verbosity)
                   when verbosity (putStrLn ("SAFETY VERIFICATION? " ++ (if valid then "OK" else "FAILED")))
       putStrLn ""
