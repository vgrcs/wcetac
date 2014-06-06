
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ACC
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Analyzer.ACC ( timeStamp, receiver, sender, defensive

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Vec as Vec
import Data.List
import Data.Word
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Cmd
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Codec.Compression.GZip as GZip
import System.IO.Unsafe
import Control.Monad

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Pipeline
import Arm.Instruction
import Analyzer.Certificate
import Analyzer.Analysis
import Analyzer.Semantics
import Analyzer.ILP
import Analyzer.Lattice
import Analyzer.Label
import Analyzer.PipelineModel
import Analyzer.ARM5StagePipeline
import Analyzer.Serializer
import Analyzer.Interpreter
import Analyzer.LPModel
import AMPL.AMPL hiding (var)
import ParserC


process
  :: (Cost t, Show t, Ord t, Ord (Coord t), Show (Node (CPU t)), Show (Core t)) =>
     Maybe (Cert (CPU t))
     -> FilePath
     -> ReductionMode
     -> Bool
     -> IO (Relations t, Relations t, Relations t, Cert (CPU t))

process cert fname mode verbosity
  = do str <- readFile fname

       (r, linear, rel, acc, original, edges) <- timeStamp verbosity "FIXPOINT SOLVER= " $
                                                 analysis str cert mode

       let edges'  = pairsToEdges edges rel
           acc_ = Map.mapWithKey (\ k n -> unsafePerformIO (convert k n)) acc

       wcets <- timeStamp verbosity "LP SOLVER= " $
                calculation rel acc_ edges' mode Supplier Nothing

       let (_,wcet, primal, dual) = head wcets

       putStrLn ("WCET= " ++ show (head $ map (\(_,w,_,_) -> w) wcets))

       let cert = Cert { invariants = acc, iterations = original,
                         edgeCount = edges, wcet = wcet, primal = primal, dual = dual }

       --okay <- tic "MODEL" $  return $
       --                       verifyModel rel acc_ edges' (wcet, primal, dual)
       --putStrLn $ "OKAY? " ++ show okay

       --let wcet1 = solveModelVars rel acc_ edges'
       --putStrLn $ "MODEL =>" ++ (show wcet1)

       return (r, linear, rel, cert)


receiver
  :: String
  -> Cert (CPU WCET)
  -> ReductionMode
  -> Bool
  -> IO Bool

receiver str cert@Cert { invariants = acc, wcet = w, primal = p, dual = d }  mode verbosity
  = do
    (r, linear, rel, acc', original, edges) <- timeStamp verbosity "FIXPOINT CHECKER= " $
                                               analysis str (Just cert) mode

    let edges' = pairsToEdges edges rel

    let solutions = Just (w, p, d)
        acc_ = Map.mapWithKey (\ k n -> unsafePerformIO (convert k n)) acc

    wcets <- timeStamp verbosity "LP CHECKER= " $
             calculation rel acc_ edges' mode Consumer solutions

    let (verify, wcet', primal', dual') = head wcets
    --let okay = verifyModel rel acc_ edges' (w, p, d)
    --putStrLn ("WCET Verification= " ++ show (w, verify)) --okay
    return verify

defensive
  :: String -> ReductionMode -> Maybe (Cert (CPU WCET))
  -> IO (String, Cert (CPU WCET))

defensive fileName rmode boot
  = do
    let sourceFile = fileName ++ ".c"
        machineFile =  fileName ++ ".s"

    exit_s <- system (" arm-gp2x-linux-gcc -O0 -S  -o" ++ show machineFile ++ " " ++ sourceFile)
    exit_o <- system (" arm-gp2x-linux-gcc -O0 -gdwarf-2 -lpthread -lrt -o " ++ fileName ++ " " ++ sourceFile)
    exit_d <- system ("dwarfdump " ++ fileName ++ " > dwarf")
    (r, linear, rel, cert) <- process boot machineFile rmode True

    str <- readFile (fileName ++ ".s")

    {-dwarf <- readFile "dwarf"
    str <- readFile (fileName ++ ".s")
    (table, invs) <- processDwarf sourceFile dwarf cert
    let program = \b -> return invs

    (c:cmnts) <- comments (fileName ++ ".c")
    let comment = commentTextWithoutMarks c
        pre = derive []
        post = derive $ parse (Contract (runWhile comment)) []
        check = fork_ *** (pre /// (program *** post)) *** verify
    print  =<< check env-}
    acc str cert rmode

acc str cert Reduced
  = do
    cert'' <- chaoticTransf [] [] cert Reduced
    serialize Reduced True (Map.size (invariants cert)) (invariants cert'')
    return (str, cert'')


acc str cert NotReduced
  = do
    cert'' <- chaoticTransf [] [] cert NotReduced
    serialize NotReduced True 0 (invariants cert'')
    return (str, cert'')


sender :: String -> ReductionMode -> Bool -> IO (String, Cert (CPU WCET))

sender fileName rmode verbose
   =  do  let sourceFile = fileName ++ ".c"
              machineFile =  fileName ++ ".s"
              outputFile = fileName ++ "_out.c"
          --exit_s <- system (" arm-gp2x-linux-gcc -O0 -S  -o" ++ show machineFile ++ " " ++ sourceFile)
          --exit_o <- system (" arm-gp2x-linux-gcc -O0 -gdwarf-2 -lpthread -lrt -o " ++ fileName ++
          --                  " " ++ sourceFile)
          --exit_d <- system ("dwarfdump " ++ fileName ++ " > dwarf")
          --dwarf <- readFile "dwarf"

          --putStrLn (show (exit_s, exit_o, exit_d))

          (r, linear, rel, cert) <- process Nothing machineFile rmode verbose
          str <- readFile machineFile
          --putStrLn "Processing DWARF"
          --(table, vars) <- processDwarf sourceFile dwarf cert
          --putStrLn "Processing Contract"
          --dbc sourceFile vars
          --backannotation fileName cert table rel

          cert' <- if rmode == Reduced
                      then chaoticTransf r rel cert rmode >>= \c ->
                           serialize rmode verbose (Map.size (invariants cert)) (invariants c) >>
                           return c
                      else serialize rmode verbose 0 (invariants cert) >>
                           return cert

          return (str, cert')


removePointAfterReduce r rel
 = let toPoints r = ((ppoint . source) r):((ppoint . sink) r):[]
       r'   = nub $ concat $ map toPoints r
       rel' = nub $ concat $ map toPoints rel
   in foldl (flip delete) r' rel'

chaoticTransf :: (Cost t, Show t, Ord t, Binary.Binary t) =>
                 [Rel (St (CPU a))] -> [Rel (St (CPU a))] ->
                 (Cert (CPU t)) -> ReductionMode -> IO (Cert (CPU t))

chaoticTransf r rel cert@Cert { invariants } rmode
 = do
   let remove = removePointAfterReduce r rel
   let keepPipeline at node
        = let cpu = value node
              cpu' = case elem at remove of
                          True -> BottomCPU
                          False -> let (parent, ac) = active cpu
                                       main = multi cpu Map.! ac
                                       dones = filter done (pipeline main)
                                       --core' =  main { pipeline = nub $ map removeStubs $ filter done (pipeline main) }
                                       core' = case rmode of
                                                    --Reduced True  -> main { pipeline = nub $ map (removeStubs True) dones }
                                                    Reduced -> main { pipeline = nub $ map (removeStubs False) dones }
                                       {-core' =  if rmode == NotReduced || rmode == (Reduced True)
                                                   then case dones of
                                                             [] -> main { pipeline = [] }
                                                             ds -> main { pipeline = nub $ map (removeStubs True) dones }
                                                                   --main { pipeline = [maximum (pipeline main)] }
                                                   else main { pipeline = nub $ map (removeStubs False) $ dones } -} --not working for any seq
                                       --core'' = if at == 22 then error $ show (length (pipeline core')) else core'
                                       multi' = Map.insert ac core' (multi cpu)
                                   in cpu { multi = multi' }

              done p@PState { coords = Coord c }
                = let  f t@AbsTaskState { task = Done st } = True
                       f _ = False
                  in or $ Vec.toList $ Vec.map f c

          in  node { value = cpu', stableFixpoint = False,
                          stableValue = [], insideLoop = No,
                          contexts = []}

       acc = Map.filterWithKey (\at c -> value c /= BottomCPU) $
             Map.mapWithKey keepPipeline invariants

   return (cert { invariants = acc} )


serialize mode verbose points acc
  = do
    --Binary.encodeFile "serial.txt" acc
    --(ByteString.writeFile "serial.txt"  .  Binary.encode) acc
    let string = (GZip.compress  .  Binary.encode) acc

    when verbose
         (putStrLn ("CERTIFICATE SIZE= " ++ show (round ((fromIntegral (ByteString.length string))/1024)) ++ "Kb"))

    when (verbose && mode == Reduced)
         (putStrLn ("REDUCTION OF PROGRAM POINTS= " ++ show points ++ " -> " ++ (show (Map.size acc))))

    (ByteString.writeFile "certificate.bin" . GZip.compress  .  Binary.encode) acc


timeStamp :: Bool -> String -> IO t -> IO t
timeStamp verbosity str a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    when verbosity (printf (str ++ " : %0.3f sec\n") (diff :: Double))
    return v
