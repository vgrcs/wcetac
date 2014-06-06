

-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Chaotic
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP #-}
module Analyzer.Chaotic ( chaotic

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import System.IO.Unsafe
import Prelude hiding (lookup)
import Data.Map
import Data.Maybe
import qualified Data.List as List
import qualified Data.Vec as Vec
import qualified Data.Array as Array
import Data.Number.PartialOrd
import qualified Control.Monad as Control
import Control.DeepSeq

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.CPU
import Arm.Memory
import Arm.Pipeline
import Arm.Loader
import Arm.Register
import Arm.RegisterName
import Analyzer.ValueAbstraction
import Analyzer.Label
import Analyzer.Certificate
import Analyzer.Lattice
import Analyzer.Container
import Analyzer.Semantics
import Analyzer.ScreenPrinter




-- | One chaotic fixpoint iteration
chaotic
   :: (Show a, Eq a, Cost a, Ord a, Transition r, Show (Core a)) => r
   -> Invs (CPU a)
   -> ACC
   -> (CPU a)
   -> IO (Invs (CPU a))

chaotic rel cert Supplier cpu
   =  do
      cpu' <- read_ cert (sink rel)

      let cpu'' = join cpu' cpu

      let cpu_ = unsafePerformIO $ do
                     if  ppoint (sink rel)  == 98
                         then  do
                               putStrLn "CHAOTIC "
                               --putStrLn $ "NEW\n" ++ show (getRegisters cpu)
                               --putStrLn $ "OLD\n" ++ show (getRegisters cpu')e
                               --putStrLn $ "JOIN\n" ++ show (getRegisters cpu'')
                               let x = getReg (getRegisters cpu) R3
                                   y = getReg (getRegisters cpu') R3
                                   z = getReg (getRegisters cpu'') R3
                               putStrLn $ "NEW\n" ++ show (multi cpu)
                               putStrLn $ "OLD\n" ++ show (multi cpu')
                               putStrLn $ "JOIN\n" ++ show (multi cpu'')

                               return cpu'
                         else return cpu'

      ret <- if cpu' == bottom
         then store cert rel cpu
         else if cpu'' /= cpu'
              then do
                   cert' <- store cert rel cpu''
                   return $ unsafePerformIO (stabilize rel cpu' cpu'' cert' False)

         else return $ unsafePerformIO (stabilize rel cpu' cpu'' cert True)

      let ret' = unsafePerformIO $ do
                     putStrLn $ "END ======================================="
                     return ret

      return ret


chaotic rel cert Consumer cpu
   =  do
      cpu' <- read_ cert (sink rel)
      let cpu'' = join cpu' cpu
          Just post = le cpu'' cpu'
          post' = unsafePerformIO $ do
                     let core' = multi cpu' ! snd (active cpu')
                         core'' = multi cpu'' ! snd (active cpu'')  -- after join
                         Registers r' = registers core'
                         Registers r'' = registers core''
                     --putStrLn $ "CONSUMER " -- ++ show (le (instrMem core') (instrMem core''))
                     --putStrLn $ show (Array.assocs r')
                     --putStrLn "###"
                     --putStrLn $ show (Array.assocs r'')
                     --putStrLn $ show post
                     return post
      if  post'
          then return $ unsafePerformIO $ stabilize rel cpu' cpu'' cert True

          else do
               cert'  <- store cert rel cpu''
               return $ unsafePerformIO $ stabilize rel cpu' cpu'' cert' False



stabilize
   :: (Show a, Eq a, Cost a, Transition r) => r
   -> CPU a
   -> CPU a
   -> Invs (CPU a)
   -> Bool
   -> IO (Invs (CPU a))

stabilize rel cpu' cpu'' cert False
  = do
    cert' <- stabilizeValue cpu'' cpu' cert rel
    cert'' <- stabilizeLoop rel cpu' cpu'' cert'
    return cert''

stabilize rel cpu' cpu'' cert True
  = do
    cert' <- stabilizeValue cpu'' cpu' cert rel
    cert'' <- stabilizeNode cert' rel cpu''
    return cert''


stabilizeLoop
   :: (Show a, Eq a, Cost a, Transition r) => r
   -> CPU a
   -> CPU a
   -> Invs (CPU a)
   -> IO (Invs (CPU a))

stabilizeLoop rel cpu' cpu'' cert
  = if (heads . source) rel
       then  do
             let  core' = multi cpu' ! snd (active cpu')
                  core'' = multi cpu'' ! snd (active cpu'')  -- after join
                  regs' = registers core'
                  regs'' = registers core''
             case (getReg regs' CPSR, getReg regs'' CPSR) of
                  (CtrVal c', CtrVal c'') -> do
                      do
                      let existing = toList $ back c'
                          new = toList $ back c''
                          f (aa, va) (ab,vb)
                                = let x = case (va `le` vb) of
                                               Just y -> y
                                               Nothing -> error $ "stabilizeLoop error: " ++ show (va,vb)
                                  in aa == ab && x
                          m = zipWith f new existing
#if defined(DEBUG)
                      putStrLn $ "Stabilize Loop " ++ show (and m, sink rel) -- show (new, existing)
#endif
                      if  and m -- back c' == back c''
                          then  return (adjust (\n -> n { stableLoop = True }) (ipoint (sink rel)) cert)
                          else  return (adjust (\n -> n { stableLoop = False }) (ipoint (sink rel)) cert)
                  _ -> return cert
       else return cert


-- | Assign the fixpoint reached flag
stabilizeNode
  :: (Transition r) => (Invs (CPU a))
  -> r
  -> CPU a
  -> IO (Invs (CPU a))

stabilizeNode cert rel cpu@CPU {context = ctxs}
   =  do
      let after = sink rel
      let f n@Node {contexts}
            = let ctxs'  = unsafePerformIO $ ifNewCContext contexts rel ctxs
                  ctxs'' = unsafePerformIO $ ifDeleteCContext contexts rel ctxs'
              in n { stableFixpoint = True,  contexts = ctxs'' ,
                     stableValue = [DataStable, MemStable, PipelineStable]   }

#if defined(DEBUG)
      putStrLn ("stabilize at " ++ show after)
#endif
      return (adjust f (ipoint after) cert)


-- | Assign the value analysis stabilization flag
stabilizeValue
   :: (Show a, Eq a, Cost a, Transition r) =>
   CPU a
   -> CPU a
   -> Invs (CPU a)
   -> r
   -> IO (Invs (CPU a))

stabilizeValue a b cert rel
   =  do
      let after = sink rel
      let coreA = multi a ! snd (active a)
          coreB = multi b ! snd (active b)
      let r = (registers coreA ) <= (registers coreB)
          im = (instrMem coreA ) <= (instrMem coreB)
          dm = datam (memory a) <= datam  (memory b)
          p = fromJust $ (pipeline coreA) `le` (pipeline coreB)

      {-let fixed n = n { stableValue = (True, Just [DataStable, MemStable, PipelineStable]) }
          dataAndMem n = n { stableValue = (False, Just [DataStable, MemStable]) }
          onlyData n = n { stableValue = (False, Just [DataStable]) }
          onlyMem n = n { stableValue = (False, Just [MemStable]) }
          onlyPipe n = n { stableValue = (False, Just [PipelineStable]) } -}

      let fixed elems n = n { stableValue = elems }

#if defined(DEBUG)
      putStrLn ("check value[" ++ show (after) ++ "]: registers= " ++ show r ++ " data= " ++ show dm ++ " pipeline= " ++ show p)
      if  ppoint after == 8
          then do
               putStrLn $ "RA= " ++ show  (registers coreA )
               putStrLn $ "RB= " ++ show  (registers coreB )
          else return ()
#endif

      {-if r && dm && im && (not p)
         then do
              putStrLn $ "PA= " ++ show (pipeline coreA)
              putStrLn $ "PB= " ++ show (pipeline coreB)
              return ()
         else return ()-}

      case (p, im, r && dm) of
           (False, False, False) -> return cert
           (False, False, True) -> return $ adjust (fixed [DataStable]) (ipoint after) cert
           (False, True, False) -> return $ adjust (fixed [MemStable]) (ipoint after) cert
           (False, True, True) -> return $ adjust (fixed [MemStable, DataStable]) (ipoint after) cert
           (True, False, False) -> return $ adjust (fixed [PipelineStable]) (ipoint after) cert
           (True, False, True) -> return $ adjust (fixed [DataStable, PipelineStable]) (ipoint after) cert
           (True, True, False) -> return $ adjust (fixed [MemStable, PipelineStable]) (ipoint after) cert
           (True, True, True) -> return $ adjust (fixed [MemStable, PipelineStable, DataStable]) (ipoint after) cert





