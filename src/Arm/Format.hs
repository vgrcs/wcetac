----------------------------------------------------------------------
-- FILE:              Format.hs
-- DATE:              03/30/2001
-- PROJECT:
-- LANGUAGE PLATFORM:
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Format
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Array
import Data.Word

----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Analyzer.ValueAbstraction
import Arm.Memory

----------------------------------------------------------------------
-- Number base data type.
----------------------------------------------------------------------
data Radix
  = Dec
  | Hex
  deriving (Show)



----------------------------------------------------------------------
-- Format a number in a specific number base.
----------------------------------------------------------------------
formatNum base
  = case base of
      Dec -> formatDec 10 '0'
      -- Hex -> formatHex 8 '0' ""



----------------------------------------------------------------------
-- Convert a number to a hex string.
----------------------------------------------------------------------
formatHex
  :: Int
  -> Char
  -> String
  -> Value
  -> String

formatHex places fillChar accum (CtrVal c@Control {control = n} )
  = if places == 0
      then accum
      else let digIndex = n `mod` 16
               dig = if n == 0
                       then fillChar
                       else hexChars ! digIndex
           in formatHex (places - 1) fillChar (dig : accum) (CtrVal c { control = (n `div` 16) } )


----------------------------------------------------------------------
-- Array of hex characters.
----------------------------------------------------------------------
hexChars
  :: Array Word32 Char

hexChars
  = listArray (0, 15) "0123456789ABCDEF"



----------------------------------------------------------------------
-- Format a decimal integer
----------------------------------------------------------------------
formatDec
  :: Int
  -> Char
  -> Value
  -> String

formatDec places fillChar (CtrVal n)
  = let s = show n
        pad = places - (length s)
    in (take pad (repeat ' ')) ++ s

formatDec places fillChar (RegVal n)
  = let s = show n
        pad = places - (length s)
    in (take pad (repeat ' ')) ++ s

formatDec places fillChar (Bottom)
  = let s = "_|_"
        pad = places - (length s)
    in (take pad (repeat ' ')) ++ s
----------------------------------------------------------------------
-- Format a string
----------------------------------------------------------------------

formatStr
  :: Int
  -> Char
  -> String
  -> String

formatStr places char str
  = let pad = places - (length str)
        fill = (take (pad `div` 2) (repeat  char))
        str' = fill ++ str ++ fill
    in if length str' `mod` 2 == 0 then str' else str'  ++ [' ']


formatStr2 places char str
  = let pad = places - (length str)
        fill = (take pad (repeat char))
    in [' '] ++ str ++ fill

formatStr3 places char str
  = let pad = places - (length str)
        fill = (take pad (repeat char))
    in str ++ fill

formatStr4 places char str
  = let pad = places - (length str)
        fill = (take pad (repeat char))
    in fill ++ str

----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
