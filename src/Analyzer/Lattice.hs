-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Lattice
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
module Analyzer.Lattice where

-----------------------------------------------------------------------------
-- System libraries.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------


-- | Lattice type class for CPU and sub-components
class Lattice a where
  bottom  :: a
  join    :: a -> a -> a
  meet    :: a -> a -> a
  meet a b
     = error ("no default implementation for <meet>")

class Initialize a where
  initial :: a


