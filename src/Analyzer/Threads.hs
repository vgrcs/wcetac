-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Threads
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

module Analyzer.Threads (

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.State

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.IR
import Analyzer.Lattice
import Analyzer.Label
import Analyzer.Semantics
import Analyzer.MOP


data ForkGraphSt a = ForkGraphSt {sync :: (Bool, Maybe (CFG a)),
                                  fork :: Bool,
                                  outer :: Bool,
                                  wait :: Bool }

instance Initialize (ForkGraphSt a) where
  initial = ForkGraphSt { fork=False, sync=(False, Nothing), outer=True, wait = False }

