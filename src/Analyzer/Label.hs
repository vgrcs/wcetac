
-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.ARM5StagePipeline
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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Analyzer.Label  ( Label (..), Labeled (..), Proc (..) , Identifier (..) ) where

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Lattice

-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import qualified Data.Map as Map
import Control.DeepSeq
import Data.Word
import Data.List.Utils


type IntlvPos = Maybe Int

data Identifier = Identifier { labelId :: Integer, procedure :: Proc, ilvpos :: IntlvPos }

instance Eq Identifier where
  a == b = labelId a == labelId b && procedure a == procedure b


data Proc = Proc { procId :: Int, section :: String, procName :: String }




instance Eq Proc where
  a == b = if startswith "." (section a) && (section b == procName b)
             then (procName a) == (procName b)
             else if startswith "." (section b) && (section a == procName a)
             then (procName a) == (procName b)
             else (procName a) == (procName b) && (section a) == (section b)


instance Show Proc where
  show p@Proc { section, procName } =  show (section, procName)

instance  Enum Identifier where
  succ i@Identifier { labelId = l } = i { labelId = succ l }
  pred i@Identifier { labelId = l } = i { labelId = pred l }
  fromEnum i = (fromInteger . labelId) i
  toEnum = error "toEnum not implemented for Identifier"


instance Initialize Identifier where
  initial = Identifier { labelId = -1 , procedure = initial, ilvpos = Nothing }

instance  Enum Proc where
  succ p@Proc { procId = i} = p { procId = succ i }
  pred p@Proc { procId = i} = p { procId = pred i }
  fromEnum p = procId p
  toEnum = error "toEnum not implemented for Proc"

instance Initialize Proc where
  initial = Proc { procId = 1 , section = "unknown", procName = "unknown" }

instance Enum Label where
  succ (Head l) = Head (succ l)
  succ (NodeL l) = NodeL (succ l)
  succ (CallL l) = HookL (succ l)
  succ (HookL l) = NodeL (succ l)

  pred (Head l) = Head (pred l)
  pred (NodeL l) = NodeL (pred l)
  pred (ExitL l) = ExitL (pred l)
  fromEnum = error "fromEnum not implemented for Label"
  toEnum = error "toEnum not implemented for Label"


data Label = Head Identifier
           | NodeL Identifier
           | ExitL Identifier
           | CallL Identifier
           | RootL Identifier
           | HookL Identifier
           | Empty


instance Ord Label where
   compare r s
     = case (pos r, pos s) of
            (Just a, Just b) -> compare a b
            _ ->  compare (ppoint r) (ppoint s)

instance Initialize Label where
  initial = NodeL initial


class Labeled a where
  ppoint  :: a -> Integer
  ipoint  :: a -> (Integer)
  parent  :: a -> Proc
  hook :: a -> Bool
  root :: a -> Bool
  heads :: a -> Bool
  bl :: a -> Bool
  exit  :: a -> Bool
  shift :: a -> Integer -> a
  setId :: a -> Integer -> a
  setPos :: a ->  Int -> a
  pos :: a -> Maybe Int
  identifier :: a -> Identifier


desc l = (labelId l)

instance Labeled Label where
  ppoint (Head l)  = labelId l
  ppoint (NodeL l) = labelId l
  ppoint (CallL l) = labelId l
  ppoint (RootL l) = labelId l
  ppoint (HookL l) = labelId l
  ppoint (ExitL l) = labelId l
  ppoint (Empty) = -1
  ipoint (Head l)  = desc l
  ipoint (NodeL l) = desc l
  ipoint (CallL l) = desc l
  ipoint (RootL l) = desc l
  ipoint (HookL l) = desc l
  ipoint (ExitL l) = desc l
  ipoint (Empty)   = (-1)
  identifier (Head l)  = l
  identifier (NodeL l) = l
  identifier (CallL l) = l
  identifier (RootL l) = l
  identifier (HookL l) = l
  identifier (ExitL l) = l
  identifier (Empty) = initial
  parent (Head  l) = procedure l
  parent (NodeL l) = procedure l
  parent (CallL l) = procedure l
  parent (RootL l) = procedure l
  parent (HookL l) = procedure l
  parent (ExitL l) = procedure l
  parent (Empty) = initial
  pos (Head  l) = ilvpos l
  pos (NodeL l) = ilvpos l
  pos (CallL l) = ilvpos l
  pos (RootL l) = ilvpos l
  pos (HookL l) = ilvpos l
  pos (ExitL l) = ilvpos l
  hook (Head _) = False
  hook (NodeL _) = False
  hook (CallL _) = False
  hook (RootL _) = False
  hook (HookL _) = True
  hook (ExitL _) = False
  heads (Head _) = True
  heads (NodeL _) = False
  heads (CallL _) = False
  heads (RootL _) = False
  heads (HookL _) = False
  heads (ExitL _) = False
  root (Head _) = False
  root (NodeL _) = False
  root (CallL _) = False
  root (RootL _) = True
  root (HookL _) = False
  root (ExitL _) = False
  bl (Head _) = False
  bl (NodeL _) = False
  bl (CallL _) = True
  bl (RootL _) = False
  bl (HookL _) = False
  bl (ExitL _) = False
  exit (Head _) = False
  exit (NodeL _) = False
  exit (CallL _) = False
  exit (RootL _) = False
  exit (HookL _) = False
  exit (ExitL _) = True
  shift (NodeL l@Identifier { labelId = i }) offset  = NodeL l { labelId = i + offset }
  shift (RootL l@Identifier { labelId = i }) offset  = RootL l { labelId = i + offset }
  shift (Head l@Identifier { labelId = i }) offset  = Head l { labelId = i + offset }
  shift (ExitL l) offset  = (ExitL l)
  shift Empty offset  = Empty

  setId (NodeL l) new  = NodeL l { labelId = new }
  setId (RootL l) new  = RootL l { labelId = new }
  setId (Head l) new   = Head l { labelId = new }
  setId (ExitL l) new  = ExitL l { labelId = new }
  setId (CallL l) new  = CallL l { labelId = new }
  setId (HookL l) new  = HookL l { labelId = new }
  setId Empty _  = Empty

  setPos (NodeL l) new  = NodeL l { ilvpos = Just new }
  setPos (RootL l) new  = RootL l { ilvpos = Just new }
  setPos (CallL l) new  = CallL l { ilvpos = Just new }
  setPos (HookL l) new  = HookL l { ilvpos = Just new }
  setPos (Head l) new   = Head l { ilvpos = Just new }
  setPos (ExitL l) new  = ExitL l { ilvpos = Just new }
  setPos Empty _ = Empty

instance Eq Label where
  Head  l1 == Head  l2    =  l1 == l2
  NodeL l1 == NodeL l2    =  l1 == l2
  Head  l1 == NodeL l2    =  l1 == l2
  NodeL l1 == Head  l2    =  l1 == l2


  ExitL x == ExitL y      =  x == y
  ExitL x == _            =  False
  _ == ExitL x            =  False

  CallL x == CallL y      =  x == y
  CallL l1 == NodeL l2    =  l1 == l2
  NodeL l1 == CallL l2    =  l1 == l2
  CallL l1 == Head  l2    =  l1 == l2
  Head  l1 == CallL l2    =  l1 == l2

  RootL l1 == NodeL l2    =  l1 == l2
  NodeL l1 == RootL l2    =  l1 == l2
  RootL l1 == RootL l2    =  l1 == l2
  RootL l1 == Head  l2    =  l1 == l2
  Head  l1 == RootL l2    =  l1 == l2

  HookL l1 == NodeL l2    =  l1 == l2
  NodeL l1 == HookL l2    =  l1 == l2
  HookL l1 == HookL l2    =  l1 == l2
  HookL l1 == Head  l2    =  l1 == l2
  Head  l1 == HookL l2    =  l1 == l2
  HookL l1 == RootL l2    =  l1 == l2
  RootL l1 == HookL l2    =  l1 == l2

  other == other2 = error ("eq ?" ++ show (other, other2))

instance Show Label where
  showsPrec 0 (Head l)    =  (("H" ++ show (labelId l)) ++)
  showsPrec _ (Head l)    =  (("H" ++ show (labelId l) ++ " {" ++ show (procedure l) ++ "}") ++)
  showsPrec 0 (NodeL l)   =  (("n" ++ show (labelId l)) ++)
  showsPrec _ (NodeL l)   =  (("n" ++ show (labelId l) ++ " {" ++ show (procedure l) ++ "}") ++)
  showsPrec _ Empty       =  ("empty label" ++)
  showsPrec _ (ExitL l) = let (s, p) = ((section . procedure) l, (procName . procedure) l)
                          in  if  s == p
                                  then (("exit "++ " {" ++ show s ++ "}") ++)
                                  else (("exit "++ " {" ++ show (s,p) ++ "}") ++)
  showsPrec _ (CallL l) = let c = (labelId l)
                              (s, p) = ((section . procedure) l, (procName . procedure) l)
                          in if  s == p
                                 then  (("c"++ show c ++ " {" ++ show s ++ "}") ++)
                                 else  (("c"++ show c ++ " {" ++ show (s,p) ++ "}") ++)
  showsPrec _ (RootL l) = let c = (labelId l)
                              n = (procId . procedure) l
                              (s, p) = ((section . procedure) l, (procName . procedure) l)
                          in if  s == p
                                 then  (("r"++ show c ++ " {" ++ show s ++ "}; " ++ show n) ++)
                                 else  (("r"++ show c ++ " {" ++ show (s,p) ++ "}; " ++ show n) ++)
  showsPrec _ (HookL l) = let c = (labelId l)
                              (s, p) = ((section . procedure) l, (procName . procedure) l)
                          in if  s == p
                                 then (("hook "++ show (c,s)) ++)
                                 else (("hook "++ show (c,(s,p))) ++)



