-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Semantics
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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Analyzer.Semantics (
  Rel (..), RelAbs (..) ,  St (..),   Abstractable (..),  TransSys (..),
  TransSys3 (..),
  Expr (..), Transition (..), Stateable (..), Infeasible (..), Color (..),
  appendExpr , pairsToEdges, toListExpr, tailExecFromList, latex,
  isInterleaved, colorfy, SimContext (..), update
) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Prelude hiding (lookup)
import Data.Word
import Data.Map hiding (update)
import qualified Data.List as List
import System.IO.Unsafe
import Control.DeepSeq
import Control.Monad hiding (join)
import Control.Monad.State hiding (lift,join)
import Data.Maybe
import qualified Data.Vec as Vec

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Arm.Instruction
import Arm.CPU
import Arm.Pipeline hiding (update)
import Arm.RegisterName
import Arm.Register
import Arm.Format
import Analyzer.Label hiding (labelId)
import Analyzer.Lattice
import Analyzer.Certificate
import Analyzer.ValueAbstraction
import Analyzer.ScreenPrinter

data Rel a = Rel (a, Expr, a)


instance (Initialize a) => Initialize (Rel a) where
  initial = Rel (initial, Exec Nop, initial)


instance (Stateable a) => Eq (Rel a) where
  r == s  =  ppoints r == ppoints s -- && expr r == expr s


instance (Stateable a) => Ord (Rel a) where
  compare r s
    =  let (b,a) = ppoints r
           (d,c) = ppoints s
           compare' p r s
             = let (cr, pr) = (ppoint r, parent r)
                   (cs, ps) = (ppoint s, parent s)
               in if p == pr && p /= ps
                     then LT
                     else  if p == ps && p /= pr
                           then GT
                     else LT -- if heads b then GT else if heads a then LT else LT
       in if (a == c) then let p = parent a
                           in compare' p b d
                      else compare a c


type RelAbs a =  a -> IO a

data TransSys a = TransSys [Rel a]
data TransSys3 a = TransSys3 (Map Int (Rel a, Bool))

data Expr = Exec Instruction
          | Threaded Instruction
          | Interleaved Instruction
          | Cons Instruction Expr

appendExpr :: Expr -> Expr -> Expr
appendExpr (Exec l) (Exec i)  = Cons l (Exec i)
appendExpr (Cons l list) (Exec i)  = Cons l (appendExpr list (Exec i))

tailExpr :: Expr -> Instruction
tailExpr (Exec i) =  i
tailExpr (Cons i l) = tailExpr l

tailExecFromList :: (Stateable a) =>  [Rel a] -> Instruction
tailExecFromList l
  = case expr (List.last l) of
         Exec i -> i
         Threaded i -> i

dropExpr (Cons i (Exec _)) = Exec i
dropExpr (Cons i l) = Cons i (dropExpr l)

toListExpr ::  Expr -> [Instruction]
toListExpr (Exec i) = [i]
toListExpr (Threaded i) = [i]
toListExpr (Interleaved i) = [i]
toListExpr (Cons i l) = (i):(toListExpr l)


isInterleaved (Interleaved _) = True
isInterleaved _ = False

data Color = RegularC | Parallel | InterleavedC | CompC | EnterC | LeaveC

colorfy Parallel (Rel (sink, Exec i, souce))
    = (Rel (sink, Threaded i, souce))

colorfy InterleavedC (Rel (sink, Exec i, souce))
    = (Rel (sink, Interleaved i, souce))

instance Show Expr where
  show (Exec i) = show i
  show (Threaded i) = show i ++ " (T)"
  show (Interleaved i) = show i ++ " (I)"
  show (Cons i l) = let last = tailExpr (Cons i l)
                        drop = dropExpr (Cons i l)
                    in -- " (" ++
                       show drop ++ " * " ++ show last
                       -- ++ ") "
  {-showsPrec 1 (Interleaved i) = (((show i) ++ " (I)") ++)
  showsPrec 1 (Exec i) = ((show i) ++)
  showsPrec 1 (Cons i l) = let last = tailExpr (Cons i l)
                               drop = dropExpr (Cons i l)
                           in ((show drop ++ " * "  ++ show last) ++)-}
  --showsPrec 0 b = ("show expr" ++)


instance Eq Expr where
   (Exec a) == (Exec b) = (show a) == (show b)
   (Cons a la) == (Cons b lb) = (show a) == (show b) && la == lb
   (Interleaved a) == (Interleaved b) = (show a) == (show b)
   (Threaded a) == (Threaded b) = (show a) == (show b)
   a == b = False


data St a = St { labelSt :: Label,
                 invs :: Invs a,
                 edges :: EdgeCount,
                 activeSt :: (Int, Int) }

instance Initialize (St a) where
  initial =  St { labelSt = initial,  invs = empty, edges = empty , activeSt = (0,0) }


data SimContext a = SimContext { simRel    :: Rel (St (CPU a)),
                                 simInstrs :: [Instruction],
                                 simCPU    :: CPU a,
                                 simTask   :: Maybe Task }


instance Show (St b) where
  showsPrec i  st@St { labelSt , invs}
     = ((show labelSt) ++)


class Abstractable a where
  --apply :: St a -> Rel (St a) -> RelAbs (Invs a) -> Par (St a)
  apply :: St a -> (Invs a -> IO (Rel (St a), Invs a)) -> IO (St a)
  lift  :: Rel (St a) -> RelAbs a -> ACC -> (Invs a) -> IO (Rel (St a), Invs a)
  --lift  :: RelAbs a -> ACC -> RelAbs (Rel (St a), Invs a)


instance Stateable (St a) where
   getLabel s = labelSt s
   setLabel s l = s { labelSt = l }
   setILvPos s@St { labelSt = l } p = s { labelSt = setPos l p }
   shiftLabel s@St { labelSt = l } i = s { labelSt = shift l i }

class Transition a where
   ppoints :: a -> (Label, Label)
   cpoints :: a -> (String, String)
   uniqueId :: a -> String
   sink :: a -> Label
   source :: a -> Label
   expr :: a -> Expr
   adjustIn :: a -> Label -> a
   adjustOut :: a -> Label -> a

class Stateable a where
   getLabel :: a -> Label
   setLabel :: a -> Label -> a
   setILvPos :: a -> Int -> a
   shiftLabel :: a -> Integer -> a


instance (Stateable a) => Transition (Rel a) where
   ppoints r =  (sink r, source r)
   cpoints r =  (\(y,x) ->  (show $ ppoint x, show $ ppoint y)) $ ppoints r
   uniqueId r = (\(y,x) -> show (ppoint x, ppoint y)) $ ppoints r
   sink (Rel (a, _,_)) = getLabel a
   source (Rel (_, _, a)) = getLabel a
   expr (Rel (_, i,_)) = i
   adjustIn (Rel (b, e, a)) label = Rel (b, e, setLabel a label)
   adjustOut (Rel (b, e, a)) label = Rel (setLabel b label, e, a)
   --compl (Rel (b, Exec e, a)) = Rel (b, Exec (complInstr e), a)


class Infeasible a where
   infeasible :: a -> IO Bool
   becomeFeasible :: a -> IO a

instance (Show b) => Infeasible (St (CPU b)) where

  infeasible s@St { labelSt = at, invs=cert }
     = do
       let Just node = lookup (ipoint at) cert
       let cpu@CPU {multi, active = (parent, child) } = value node
           core = multi ! child
       return $ infeasiblePath (registers core)

  becomeFeasible s@St { labelSt = at, invs=cert }
     = do
       let Just node = lookup (ipoint at) cert
       let cpu@CPU{ multi, active = (parent, child) } = value node
       let core = multi ! child
           CtrVal status@Control { control = cpsr } = getReg (registers core) CPSR
           clear = status { control = clearControlI (control status) }
           regs = setReg (registers core) CPSR (CtrVal clear)
           core' = core { registers = regs  }
           multi' = insert child core' multi
       let cpu' = cpu { multi = multi' }
       let node'  = node { value = cpu' }
       let cert' = insert (ipoint at) node' cert

           cert'' = unsafePerformIO $ do
                    putStrLn $ "becomeFeasible " ++ show (at, cpu')
                    return cert'

       return s { invs = cert'}


pairsToEdges
  :: (Stateable a) =>  EdgeCount
  -> [Rel a]
  -> Map Integer Loop

pairsToEdges lns rel
  = let  f (start, end)
            = let g r = if (ppoint.source) r == start && (ppoint.sink) r == end && (not (exit (sink r)))
                           then True
                           else if (ppoint.source) r == start && (exit (sink r))
                                then True
                                else False
                  in case List.findIndex g rel of
                          Just ix -> toInteger ix
                          Nothing -> error ("pairsToEdges " ++ show (start, end) ++ "\n" ++ show lns)

    in mapKeys f lns



instance (Show a, Eq a, Cost a, Ord a) => Lattice (Invs (CPU a)) where
    bottom = let state = Node { value = bottom, stableFixpoint = False,
                                stableValue = [] , insideLoop = No,
                                contexts = [], redirect = Nothing, stableLoop = False,
                                infeasibleNode = False }
             in fromList [(0, state)]
    join a b
       = unionWithKey f a b
         where f k a b  =
                 if a== b then b
                    else  let fixpoint = stableFixpoint a && stableFixpoint b
                              loopFix = stableLoop a && stableLoop b
                              stable = stableValue a `List.union` stableValue b
                              inLoop = join (insideLoop a) (insideLoop b)
                              val = join (value b) (value a)
                              ctxs = List.union (contexts a) (contexts b)
                              rdts = case (redirect a, redirect b) of
                                          (Nothing, Nothing) -> Nothing
                                          (Nothing, Just r) -> Just r
                                          (Just r, Nothing) -> Just r
                                          (Just r1, Just r2) -> Just (List.union r1 r2)
                              infeasible = infeasibleNode a && infeasibleNode b
                              debug = unsafePerformIO $ do
                                      do
                                      let at = k
                                      if True --at == 18
                                         then do
                                              putStrLn "################################"
                                              putStrLn $ "JOINPAR " ++ show (at)
                                              putStrLn $ show (value a)
                                              putStrLn "################################"
                                              putStrLn $ show (value b)
                                              putStrLn "################################"
                                              return val
                                         else return val

                          in (Node { value = val, --alternative
                                     stableFixpoint = fixpoint, stableLoop = loopFix,
                                     stableValue = stable, insideLoop= inLoop,
                                     contexts = ctxs, redirect = rdts,
                                     infeasibleNode = infeasible } )

update
  :: (Show a) => (Task, (Pipeline a)) -> StateT (SimContext a) IO (CPU a)

update  (task, pipe)
  = do
    cpu@CPU{ multi, active = (parent, child) } <- gets simCPU
    if not $ member child multi
          then error "child not found <UpdateCPU>"
          else return ()

    let r = taskRegisters task
    let infeasible = infeasiblePath r

    let core = multi ! child
        r' = case getReg r CPSR of
                  CtrVal status@Control { control = cpsr } ->
                          let  status' = status { control = clearControlC cpsr }
                          in setReg r CPSR (CtrVal status')
                  _ -> r
        core' = core { registers = r, pipeline = pipe, instrMem = taskMemory task }
        multi' = insert child core' multi

    return $ cpu { memory = taskShared task, multi = multi'  }



instance (Show a, Eq a, Cost a, Ord a) => Lattice (St (CPU a)) where
   join a b
     =  let le' = unionWith (\l1 l2 -> max l1 l2) (edges a) (edges b)
            acc' = join (invs b) (invs a)
        in (a { invs = acc',  edges = le'})
   bottom = error ("bottom not necessary for states")



instance Stateable a =>  Show (TransSys a) where
   show (TransSys a)
      = fst $ List.foldl
            (\(str,(p,l, offset)) r ->
                               let s = sink r
                                   s'= source r
                                   i = expr r
                                   len = length (toListExpr i)
                                   pos = case List.elemIndex r a of
                                              Just p -> p
                                              Nothing  -> 0
                                   flag st = case pos of
                                                 0 -> 1
                                                 _ -> let  prev = a !! (pos-1)
                                                           p1@Proc {section=s1, procName=n1} = parent st
                                                           p2@Proc {section=s2, procName=n2} = (parent . source) prev
                                                     in if s1 == s2 && n1 == n2 then 0 else 1

                                   l' = --l + (4*offset) --
                                        if p == ppoint s' then l  else l + (4*offset)
                                   fsink = case (flag s', flag s) of
                                                (1, 1) -> 0
                                                (_, f) -> f
                              in (str ++ ( (formatStr4 30 ' ' (showsPrec fsink s "")) ++ " <--| " ++
                                          (formatStr2 35 ' ' (show i)) ++ "<--| " ++
                                          (formatStr3 30 ' ' (showsPrec (flag s') s' "")) ++ -- "\n")
                                          (formatStr 8 ' ' ("[d"++show pos++"]")) ++
                                          (formatStr 4 ' ' ("pc= "++show l')) ++ "\n")
                                 , (ppoint s', l', len))
            ) ("", (0,0,1)) a


latex (TransSys a)
   = fst $ List.foldl
                      (\(str,(p,l)) r ->let s = sink r
                                            s'= source r
                                            i = expr r
                                            pos = case List.elemIndex r a of
                                                       Just p -> p
                                                       Nothing  -> 0
                                            flag st = case pos of
                                                           0 -> 1
                                                           _ -> let  prev = a !! (pos-1)
                                                                     p1@Proc {section=s1, procName=n1} = parent st
                                                                     p2@Proc {section=s2, procName=n2} = (parent . source) prev
                                                               in if s1 == s2 && n1 == n2 then 0 else 1

                                            l' = if p == ppoint s' then l else l+4
                                            fsink = case (flag s', flag s) of
                                                         (1, 1) -> 0
                                                         (_, f) -> f
                                        in (str ++ "\\textbf{ " ++ (showsPrec fsink s "") ++ "}:&" ++
                                            "\\texttt{" ++ show i ++ "}"  ++
                                            "& :\\textbf{" ++ (showsPrec (flag s') s' "") ++ "} & \\\\ \n"
                                            , (ppoint s', l'))
                                        --in (str ++ ( (formatStr4 23 ' ' (showsPrec fsink s "")) ++
                                        --            (formatStr2 27 ' ' (" " ++ show i)) ++
                                        --            (formatStr3 20 ' ' (showsPrec (flag s') s' "")) ++ "\n")
                                        --    , (ppoint s', l'))
                       ) ("", (0,0)) a

latex2 rel
   =  let top = "\\scriptsize \n" ++ "\\begin{tabular} {r l l l}\n"
          bot = "\\end{tabular}" ++ "\\normalsize\n"
      in top ++ (latex rel) ++ bot
