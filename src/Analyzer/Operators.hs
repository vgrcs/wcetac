
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Operators
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

module Analyzer.Operators ( copy, (****), (++++), ($$$$), (////), wide, multiply, divide, reduce

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Monad hiding (join)
import System.IO.Unsafe
import Control.Monad.Fix

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.ProgramFlow
import Analyzer.Lattice
import Analyzer.Semantics
import Analyzer.Interleavings

copy
  :: (Forkable a) => a
  -> IO (a,a)

copy a
  = do
    a' <- compl a
    return (a, a')


divide
  :: Int -> a -> IO [a]

divide n a
  = do
    return $ replicate n a

multiply
  :: (Lattice a, Show a, Synchronizable a) => [a -> IO a]
  ->  [a]
  ->  IO [a]

multiply fs as
  = do
    zipWithM (\f a -> f a) fs as

reduce
  :: (Lattice a, Synchronizable a, Show a) => [a]
  ->  IO a

reduce (a:as)
  = do
    (a':as') <- mapM finalize (a:as)
    return $ foldl join a' as'


(*****) :: (a -> b) -> (b -> c) -> (a -> c)
(f ***** g) s = (g . f) s

(/////) :: (a -> b) -> (c -> d) -> ((a,c) -> (b, d))
(f ///// g) (s, t) = (f s, g t)


(****)
   :: (Show a, Show b) =>(a -> IO b)
   -> (b -> IO c)
   -> a
   -> IO c


(f **** g) s
   = do
     s' <- f s
     s'' <- g s'
     return s''

(++++)
   :: (Show b, Forkable a, Iterable b, Stateable a) =>  (b -> IO a)
   -> (a -> IO b)
   -> a
   -> IO a

f ++++ t
  =  fix $
     \rec s -> do
               s' <- t s
               b  <- fixpoint s'
               if   b  then (loop s **** f **** rec) s'
                       else compl s



($$$$)
  :: (Iterable b, Iterable c, Stateable b) =>
     (c -> IO b) -> (b -> IO c) -> b -> IO c

f $$$$ t
  =  fix $
     \rec s -> do
               s' <- t s
               b  <- emptyStack s
               if  not b
                   then (loop s **** f **** rec) s'
                   else return s'

(////)
   :: (Show a, Show c) => (a -> IO  b)
   -> (c -> IO  d)
   -> (a,c)
   -> IO (b, d)

(f //// g) (s, t)
   = liftM2 (\ x y -> (x,y)) (f s) (g t)


wide
   :: (Show a, Stateable a, Lattice a, Infeasible a) => (Rel a) -> (a, a)
   -> IO  a

wide r (a, b)
   = do
     ia <- infeasible a
     ib <- infeasible b

     j <- case (ia, ib) of
          (False, False) -> return $ join a b
          (False, True)  -> do
                            a' <- becomeFeasible a
                            b' <- becomeFeasible b
                            return $ join a' b'
          (True, False) -> do
                           a' <- becomeFeasible a
                           b' <- becomeFeasible b
                           return $ join a' b'
          (True, True)  -> return $ join a b

     return j
