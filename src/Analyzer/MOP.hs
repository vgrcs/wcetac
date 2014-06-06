
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.MOP
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
module Analyzer.MOP ( MOPState (..), mop, mopStartLabel

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Monad.State
import Data.List
import Prelude hiding (seq)

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.Lattice
import Analyzer.Label
import Analyzer.IR



type Id = Label -- Maybe Integer

data MOPState a = MOPState { relational :: [Rel a],
                             startLabel :: Id, stopLabel :: Id, lastLabel :: Id,
                             loops :: [Id], insideLoop :: Bool,
                             calls :: [Id],
                             debug :: Bool,
                             threads :: [(Label, CFG a)],
                             --offset :: Integer,
                             noConc :: Bool }

instance Show (MOPState a) where
  show s = "startLabel= " ++ show (startLabel s) ++ "\n"  ++
           "stopLabel= " ++ show (stopLabel s) ++ "\n" ++
           "loops= " ++ show (loops s) ++ "\n" ++
           "insideLoop= " ++ show (insideLoop s) ++ "\n"

mopStartLabel
  :: Stateable a =>
     [Rel a] -> Label

mopStartLabel rel
  = let f r = case source r of
                   RootL l@Identifier { procedure } ->
                     case (section procedure, procName procedure) of
                          ("main","main") -> True
                          --("icrc","icrc") -> True
                          _ -> False
                   _ -> False
        index = case findIndex f rel of
                     Just i -> i
                     Nothing -> 0
        Rel (_,_,l) = rel !! index
    in getLabel l



instance Initialize (MOPState a) where
  initial = MOPState { relational = [], startLabel = initial, stopLabel = Empty, lastLabel = Empty,
                       loops = [], insideLoop = False, calls = [], debug = False, threads = [],
                       noConc = True  }



seq :: CFG a -> CFG a -> (CFG a)
seq graph new = (Seq graph new)

rec :: CFG a -> CFG a  -> (CFG a)
rec test body =  (Rec test body)

iter :: CFG a -> CFG a  -> (CFG a)
iter ldmfd body =  (Iter ldmfd body)

mop
   :: (Show a,  Stateable a) =>
   CFG a -> StateT (MOPState a) IO (CFG a)

mop s
   = do
     state <- get
     --off <- gets offset
     --start <- liftM (\l -> (shift l off)) (gets startLabel)
     --stop <- liftM (\l -> (shift l off)) (gets stopLabel)
     --cs <- liftM (\l -> map (\x -> shift x off) l) (gets calls)
     --last <- liftM (\l -> (shift l off)) (gets lastLabel)
     start <- gets startLabel
     stop <- gets stopLabel
     cs <- gets calls
     last <- gets lastLabel

     --put $ state {startLabel = start, stopLabel = stop, calls = cs, lastLabel = last}

     dbg <- gets debug
     c <- control

     --d <- gets startLabel
     --if ppoint d == 114
     --   then modify (\ state@MOPState {} -> state { debug = True } )
     --   else return ()

     case dbg of
          True ->  liftIO $ putStrLn ("\n" ++ show state ++ "\n" ++ show c ++ "\n" )
          --True ->  liftIO $ putStrLn ("\n" ++ show c ++ "\n" )
          False -> return ()

     case  c  of
           Break -> return s
           Halt ->  return s
           Step i1 ->  sequential i1 s
           Branch is -> multiple is s >>= mop

sequential
  :: (Show a, Stateable a) =>
      Int -> CFG a -> StateT (MOPState a)  IO (CFG a)

sequential i1 s
   = do
     --state <- get
     --off <- gets offset
     --liftIO $ putStrLn ("sequential, offset=" ++ show off)
     rec <- gets insideLoop
     rel <- gets relational
     start <- gets startLabel
     stop <- gets stopLabel
     cs <- gets calls
     last <- gets lastLabel

     let  r = rel !! i1
          after' = sink r
          s' = s `seq` (Leaf r)
          isRec = if rec then rec
                         else if   (ppoint after') > 0
                              then (ppoint start) > (ppoint after')
                              else False

     if (ppoint after') > (ppoint last)
        then modify (\ state@MOPState {} -> state { startLabel = after', lastLabel = after', insideLoop = isRec } )
        else modify (\ state@MOPState {} -> state { startLabel = after', insideLoop = isRec } )

     if (ppoint after') == (ppoint stop)
        then return s'
        else  case after' of
                   CallL x ->
                         do
                         let start' = succ start
                         modify (\ state@MOPState {} -> state {calls = cs ++ start':[] } )
                         s'' <- mop s'

                         --liftIO $ putStrLn $ "CALLING [" ++ show (CallL x) ++ "] START'= " ++ show start' ++
                         --                    ", STOP= " ++ show start' ++ ", CONTINUE? " ++
                         --                    "PARENT START'= " ++ show (parent start') ++
                         --                    "PARENT STOP= " ++ show (parent (CallL x)) ++
                         --                    show ((parent start' == parent (CallL x)))

                         case parent start' == parent (CallL x) of
                              False -> do
                                      modify (\ state@MOPState {} -> state {startLabel = start' } )
                                      mop s''
                              True -> return s''

                   _  -> mop s'

threadState rel
  = initial {  relational = rel, startLabel = initial, stopLabel = mopStartLabel rel }


multiple :: (Show a,  Stateable a) =>
            (Int,Int) -> CFG a -> StateT (MOPState a)  IO (CFG a)

multiple (i1,i2) state
   = do
     rel <- gets relational
     let r = rel !! i1
         s = rel !! i2
         (b1, head) = ppoints r

     --liftIO $ putStrLn ("test multiple: " ++ show r ++  " or " ++ show s)
     --liftIO $ putStrLn ("head: " ++ show head)
     case head of
          Head _ -> if  (ppoint b1) < (ppoint  head)
                        then recursive (i1,i2) state
                        else parallel (i1,i2) state
          _ -> onepass (i1,i2) state


onepass :: (Show a,  Stateable a) =>
             (Int,Int) ->  CFG a -> StateT (MOPState a) IO (CFG a)

onepass (i1,i2) arg
  = do
    state <- get
    cs <- gets calls
    let rel = relational state
    let r = rel !! i1
        s = rel !! i2
        (_,head) = ppoints r
        (site2:site1:_) = reverse cs
        start = site2
        stop = head

    --liftIO $ putStrLn "inside onepass"
    modify (\ state@MOPState {} -> state {startLabel = start, stopLabel = stop } )
    inner <- mop EmptyGraph


    let loop =  (Leaf r) `seq` inner

    --liftIO $ putStrLn ("arg= " ++ show (arg) ++ "\nS= " ++ show (expr s) ++ "\nINNER= " ++ show inner )

    modify (\ state@MOPState {} -> state {startLabel = Empty, stopLabel = Empty} )
    return $ arg `seq` ((Leaf s) `iter` inner) `seq` (Leaf r)


recursive :: (Show a,  Stateable a) =>
             (Int,Int) ->  CFG a -> StateT (MOPState a) IO (CFG a)

recursive (i1,i2) arg
   = do
     state <- get
     prevStop <- gets stopLabel
     lps <- gets loops
     let rel = relational state
     let r = rel !! i1
         s = rel !! i2
         (b1,head) = ppoints r
         (b2,head_) = ppoints s
         start = b1
         stop = head
         start' = b2


     --liftIO $ putStrLn "\ninside recursive"
     --liftIO $ putStrLn $ "head= " ++ show (expr r, ppoints r)
     --liftIO $ putStrLn $ "cont= " ++ show (expr s, ppoints s)

     --liftIO $ putStrLn $ "loops= " ++ show lps
     modify (\ state@MOPState {} -> state {startLabel = start, stopLabel = stop, loops = lps ++ stop:[]} )
     --liftIO $ putStrLn $ "new loops= " ++ show (lps ++ stop:[])


     inner <- mop EmptyGraph
     let body = (Leaf r) `rec` inner

     let lps' = delete head lps
         stop' = case lps' of
                     [] -> Empty
                     xs -> Data.List.last lps'


     --liftIO $ putStrLn $ "reset loops= " ++ show (lps')
     modify (\ state@MOPState {} -> state {startLabel = start', stopLabel = stop', loops = lps'} )

     return $ arg `seq` body `seq` (Leaf s)



parallel :: (Show a,  Stateable a) =>
            (Int,Int) ->  CFG a -> StateT (MOPState a) IO (CFG a)

parallel (i1,i2) arg
   = do
     state <- get
     --off <- gets offset
     --prevStop <- liftM (\l -> (shift l off)) (gets stopLabel)
     prevStop <- gets stopLabel
     let rel = relational state
     let r = rel !! i1
         s = rel !! i2
         (b1, head) = ppoints r
         (b2, fork)   = ppoints s
         startLft = b1
         startRht = b2
         stop = head

     --liftIO $ putStrLn "\ninside parallel"
     --liftIO $ putStrLn $ "left= " ++ show (expr r, ppoints r)
     --liftIO $ putStrLn $ "right= " ++ show (expr s, ppoints s)

     lps <- gets loops
     let  break = if  not (null lps) && b1 > last lps
                      then True
                      else False
     --liftIO $ putStrLn $ "loops= " ++ show lps
     --liftIO $ putStrLn $ "break= " ++ show break

     --modify (\ state@MOPState {} -> state { debug = True } )

     modify (\ state@MOPState {} -> state {startLabel = startLft, stopLabel = stop, debug = False } )
     lft <- mop EmptyGraph


     --lft <- mop (Leaf r) -- TODO works for sample4
     inside <- gets insideLoop
     --liftIO $ putStrLn ("\nparallel body left [" ++ show (inside) ++ "] \n=> " ++ show lft ++ "\n")

     --liftIO $ putStrLn ("INSIDE PARALLEL " ++ show inside ++ ", ST_LFT= " ++ show startLft ++ ", ST_RHT= " ++ show startRht)

     modify (\ state@MOPState {} -> state {startLabel = startRht, stopLabel = stop, debug = False } )
     rht <- mop EmptyGraph
     modify (\ state@MOPState {} -> state { debug = False } )
     --rht <- mop (Leaf s)
     --liftIO $ putStrLn ("\nparallel body right  \n => " ++ show rht ++ "\n")

     if not inside
        then do

             let lft_labels = labels [] lft
                 rht_labels = labels [] rht
                 join_labels = intersect lft_labels rht_labels
             --liftIO $ putStrLn ("\nNOT INSIDE  \n => JOIN= " ++ show join_labels ++ "\n")
             case join_labels of
                  j:js -> do

                         lft' <- liftIO $ cut False j (normalize lft)
                         rht' <- liftIO $ cut False j (normalize rht)

                         let lft'' = Seq (Leaf r) lft'
                             rht'' = Seq (Leaf s) rht'

                         modify (\ state@MOPState {} -> state {lastLabel = j } )
                         modify (\ state@MOPState {lastLabel} -> state {startLabel = lastLabel, stopLabel = prevStop} )

                         --error $ show (noConc state)

                         if (noConc state) --TODO
                            then if not break
                                    then return $ arg `seq` (Choice r lft'  rht'')
                                    else return $ arg `seq` (Choice r EmptyGraph  rht'')

                            else return $ arg `seq` (Conc r lft'  rht'')
                  [] -> do
                       --error $ "AHAH " ++ show "\nLFT= " ++ show lft ++ "\nRGT= " ++ show rht

                       let lft' = Seq (Leaf r) lft
                           rht' = Seq (Leaf s) rht

                       if   (noConc state)
                            then if break
                                   then return $ arg `seq` (Choice r EmptyGraph rht')
                                   else return $ arg `seq` (Choice r lft rht')
                            else return $ arg `seq` (Conc r lft rht')


        else do
             modify (\ state@MOPState {} -> state { stopLabel = prevStop } )
             --liftIO $ putStrLn ("\nINSIDE LOOP \n => ")
             --liftIO $ putStrLn ("\nLEFT \n => " ++ show lft)
             --liftIO $ putStrLn ("\nRIGHT \n => " ++ show rht)
             return $ arg `seq` ((Leaf r) `rec` lft `seq` (Leaf s) `seq` rht)



data Control = CallStep Int | Step Int | Branch (Int, Int) | Halt | Break deriving Show


control
  :: Stateable a => StateT (MOPState a) IO Control
control
  =  do
     rel <- gets relational
     start <- gets startLabel
     stop <- gets stopLabel
     lps <- gets loops
     let lps' = map ppoint lps
     let  fetch l r  =  if ppoint l == (ppoint . source) r then True else False
          fetch_ l r =  case (pos l, (pos . source) r) of
                             (Just a, Just b) -> error $ "control 1 " ++  (show (a,b))
                             _ -> False
          ix = findIndices (fetch start) rel
          ix_ = findIndices (fetch_ start) rel
     if  (ppoint start) == (ppoint stop)
         then return Break
         else
         case  ix of
               [] -> return Halt
               [i] -> return (Step i)
               --(i1:i2:[]) -> case elem start lps of
               (i1:i2:[]) -> case elem (ppoint start) lps' of
                                  False -> do
                                          --liftIO $ putStrLn $ "loop found " ++ show (start, lps)
                                          return (Branch (i1,i2))
                                  True -> do
                                         --liftIO $ putStrLn $ "NO loop found " ++ show (ppoint start, lps')
                                         --return (Step i2)
                                         return Halt
               other -> error $ "control 2 " ++ show other





