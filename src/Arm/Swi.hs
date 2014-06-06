----------------------------------------------------------------------
-- FILE:              Swi.hs
-- DESCRIPTION:
-- DATE:              03/22/2001
-- PROJECT:
-- LANGUAGE PLATFORM:
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Swi
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Char
import Data.Word
import System.IO.Unsafe

----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.LRU
import Arm.DataTypes
import Arm.CPU
import Arm.Loader
import Arm.Memory
import Arm.Register
import Arm.RegisterName
import Semantics

line = "----------------------------------------"

----------------------------------------------------------------------
-- Software interrupt services.
----------------------------------------------------------------------
swi
  :: CPU
  -> Word32
  -> Bool
  -> IO ()

-- display character in R0
swi cpu 0 debug
  = do
    let regs = registers cpu
    Std r0 <- get regs R0
    let c = fromIntegral r0
    if debug
       then putStrLn ("\n" ++ "CHR: [" ++ [chr c] ++ "]" ++ "\n")
       else putStrLn [chr c]

-- display integer in R0
swi cpu 1 debug
  =  do
     let regs = registers cpu
     Std r0 <- get regs R0
     let r0i = fromIntegral r0
     if debug
        then putStrLn ("\n" ++ "INT: [" ++ show r0i ++ "]" ++ "\n")
        else putStrLn $ show r0i

-- display string starting in location contained in R0
swi cpu 2 debug
  = do
    let regs = registers cpu
    Std r0 <- get regs R0
    let str = fetchString (memory cpu) r0
    if debug
       then putStrLn ("\n" ++ "STR: [" ++ (show str) ++ "]" ++ "\n")
       else putStrLn (show str)


-- display newline
swi cpu 10 debug
  = if debug
       then putStrLn ("\n" ++ "NEWLINE" ++ "\n")
       else putStrLn  ""

-- exit
swi cpu 11 debug
  = if debug
       then putStrLn ("\n" ++ line ++ "\n" ++ "NORMAL EXIT" ++ "\n" ++ line ++ "\n")
       else putStrLn ""

----------------------------------------------------------------------
-- Fetch a string from memory.
----------------------------------------------------------------------
fetchString
  :: Memory
  -> Address
  -> String

fetchString mem addr
  = do let (AbsValue (word,_), mem') = unsafePerformIO $ readMemWord mem addr
       let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
       let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
       let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
       let c1 = fromIntegral (word .&. 0xFF)
       if c1 == 0
         then ""
         else if c2 == 0
                then [chr c1]
                else if c3 == 0
                       then [chr c1, chr c2]
                       else if c4 == 0
                              then  [chr c1, chr c2, chr c3]
                              else do let s = fetchString mem (addr + 4)
                                      ([chr c1, chr c2, chr c3, chr c4] ++ s)




----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
