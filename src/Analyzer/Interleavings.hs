-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Interleavings
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE FlexibleInstances #-}
module Analyzer.Interleavings ( Synchronizable (..)

) where

import Analyzer.Semantics
import Analyzer.Label
import Analyzer.Lattice
import Analyzer.Certificate
import Arm.CPU
import Arm.Pipeline
import Arm.Loader
import Data.Map
import Data.Maybe
import Data.Word
import qualified Data.List as List
import Prelude hiding (lookup)
import System.IO.Unsafe


class Synchronizable a where
  create :: [Rel a] -> a -> IO a
  continue :: a -> IO a
  continue2 :: Rel a -> a -> IO a
  break :: a -> IO a
  break2 :: Rel a -> a -> IO a
  finalize :: a -> IO a
  running :: a -> (Int, Int)


instance (Cost b, Ord b, Show b) => Synchronizable (St (CPU b)) where

  running s = activeSt s

  create thread s@St { labelSt = at, invs=cert}
    =  do
       let --Just hpoint = List.findIndex (hook . source) thread
           --exit = thread !! hpoint
           --pc = (fromInteger . ppoint . source) exit
           entry = (ipoint . source) (head thread)
           Just node = lookup entry cert


       let cpu = value node
       let cpu' = unsafePerformIO $
                  resetPipelineAt cpu (fromIntegral (entry * 4) :: Word32) 0 1

       let node'  = node { value =  cpu' }
       let cert' = insert entry node' cert

       let new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "CREATE :" ++ show (entry, (0,0))
                     putStrLn "###################################"
                     --putStrLn $ show (x)
                     --error $ show (sink exit)
                     return (0,0)

       --return s { invs = cert', syncs = ( sink exit , sync) : waits, activeSt = new }
       --return s { invs = cert', activeSt = new }
       return s { invs = cert', activeSt = (0,0) }
       --return s { threadPc = Just pc}

  continue s@St { labelSt = at, invs=cert }
     = do
        if isNothing $ lookup (ipoint at) cert
           then return s
           else do
                let Just node = lookup (ipoint at) cert

                let cpu = value node

                {-let new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "CONTINUE :" ++ show (active cpu)
                     putStrLn "###################################"
                     return (0,1)-}

                let cpu' = continuePipelineAt cpu
                let node'  = node { value = cpu' }
                let cert' = insert (ipoint at) node' cert
                --return s { invs = cert', activeSt = new }
                return s { invs = cert', activeSt = (0,1) }

  continue2 r s@St { labelSt = at, invs=cert }
     = do
        if isNothing $ lookup (ipoint at) cert
           then return s
           else do
                let Just node = lookup (ipoint at) cert

                let cpu = value node

                let new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "CONTINUE :" ++ show (active cpu) ++ " " ++ show (expr r)
                     putStrLn "###################################"
                     return (0,0)


                case (isInterleaved . expr) r of
                     True -> do
                             --error $ show (active cpu)
                             let cpu' = continuePipelineAt cpu
                             let node'  = node { value = cpu' }
                             let cert' = insert (ipoint at) node' cert
                             --return s { invs = cert', activeSt = new }
                             return s { invs = cert', activeSt = (0,0) }
                     False -> return s


  break s@St { labelSt = at, invs=cert, activeSt = running }
     = do
        let Just node = lookup (ipoint at) cert

        let cpu = value node
        --let update cpu@CPU { active = (parent, child) } = cpu { active = (0, 0) }

        --error (show (at, context cpu))
        {- new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "BREAK :" ++ show (active cpu)
                     putStrLn "###################################"
                     --putStrLn $ show (TransSys thread)
                     return (0,0)-}

        let cpu' = breakPipelineAt (cpu { active = running} )
        let node'  = node { value = cpu' }
        let cert' = insert (ipoint at) node' cert
        --error $ "BREAK at " ++ show (activeSt s)
        --return s { invs = cert', activeSt = (0,0) }
        return s { invs = cert', activeSt = (0,0) }

  break2 r s@St { labelSt = at, invs=cert, activeSt = running }
     = do
        let Just node = lookup (ipoint at) cert
        let cpu = value node
        let new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "BREAK :" ++ show (active cpu)
                     putStrLn "###################################"
                     --putStrLn $ show (TransSys thread)
                     return (0,1)

        case (isInterleaved . expr) r of
             False -> return s
             True  -> do
                      let cpu' = breakPipelineAt (cpu { active = running})
                      let node'  = node { value = cpu' }
                      let cert' = insert (ipoint at) node' cert
                      --return s { invs = cert', activeSt = new }
                      return s { invs = cert', activeSt = (0,1) }

  finalize s@St { labelSt = at, invs=cert}
     = do
       let Just node = lookup (ipoint at) cert
       let cpu = value node
       let cores = multi cpu
           cores' = join (cores ! 0) (cores ! 1)
           multi' = insert 0 cores' cores
       let cpu' = cpu { multi = multi', active = (0,0) }
       let node'  = node { value = cpu'  }
       let cert' = insert (ipoint at) node' cert


       let new = unsafePerformIO $ do
                     putStrLn "###################################"
                     putStrLn $ "FINALIZE"
                     let pipe0 = pipeline (cores ! 0)
                     let pipe1 = pipeline (cores ! 1)
                     let pipe  = join pipe0 pipe1
                     putStrLn $ show pipe
                     --putStrLn "###################################"
                     --putStrLn $ show pipe1
                     --putStrLn $ show (pipeline (multi' ! 0))
                     putStrLn "###################################"
                     --putStrLn $ show (TransSys thread)
                     return cert'
       --error (show at)

       --return $ s { invs = new, activeSt = (0,0) }
       return $ s { invs = cert', activeSt = (0,0) }

resetPipelineAt
   :: (Cost a, Show a, Ord a) => (CPU a)
   ->  Word32
   -> Int
   -> Int
   -> IO (CPU a)

resetPipelineAt cpu@CPU { memory } pc owner thief
   = do
     let thiefSt = if  member (thief) (multi cpu)
                       then (multi cpu) ! (thief)
                       else bottom
         ownerSt = (multi cpu) ! (owner)
         p = pipeline ownerSt
         r = registers ownerSt
         state  =  List.maximum p
         curr = simtime state --(absolute . maxcycles) (coords state)

         p' = resetPipeline pc (thief, curr) ( instrMem ownerSt ) memory r
         thiefSt' = thiefSt { pipeline = p', instrMem = instrMem ownerSt, registers = registers ownerSt }

         multi' = insert (thief) thiefSt' (multi cpu)

     --putStrLn $ "RESET PIPELINE " -- ++ "OWNER REGS= " ++ show r

     return cpu { multi = multi'}

continuePipelineAt
  :: (Cost a, Show a, Ord a) => CPU a
  ->  CPU a

continuePipelineAt cpu@CPU { multi, active = (0, 1) }
  = setSimTime cpu 0 (getSimTime cpu 1)

continuePipelineAt cpu@CPU { multi, active }
  = error $ "continuePipelineAt " ++ show ( active)

breakPipelineAt cpu@CPU { multi, active = (0, 0) }
  = (setSimTime cpu 1 (getSimTime cpu 0))  { active = (0,1) }


getSimTime cpu@CPU { multi } target
  = let core@Core {pipeline}
          = if member target multi
                   then multi ! target
                   else error $ "child not found getSimTime "
    in simtime $ maximum pipeline


setSimTime cpu@CPU { multi } target time
  = if member target multi
       then
       let core@Core {pipeline} = multi ! target
           l:ls = reverse pipeline
           l' = l { simtime = time }
           multi' = insert target (core { pipeline = ls ++ l':[] }) multi
       in cpu { multi = multi' }
       else
       cpu

