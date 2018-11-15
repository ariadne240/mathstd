-- The Fwf function, Fwf
module Fwf
( fwf, fvbpf
) where

import System.IO
import Data.Char
import Fancyput
import Cftch
import Cftdef
import Vbcftdef

-- function fwf
fwf :: IO()
fwf = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putconch "cft> "
 putStrLn "Insert your expression."
 y <- putconch "exp> "
 putStrLn "[Result] Your wf:"
 print $ towf (cftch x) y
 putStrLn ""
-- function fpf
-- fpf :: IO()
-- function fvbpf
fvbpf :: IO()
fvbpf = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putconch "cft> "
 putStrLn "How many premises and to-be-proved expressions?"
 m2 <- putconch "prem> "
 m2 <- reptm2
 let m = read m2
 n2 <- putconch "exp> "
 n2 <- reptn2
 let n = read n2
 if (n <= 0)
 then do
       putStrLn "Error: You should put a positive integer."
 else return ()
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate (2*n) (putconch "exp> "))
 let midvbfp3st = makepfful premises tbproved
 let midvbfp4st = tovbpf34 (cftch x) midvbfp3st
 if (vvbpf (cftch x) midvbfp4st)
  then putStrLn "[Result] This proof is valid."
  else putStrLn "[Result] This proof is invalid."
 putStrLn ""
reptm2 :: IO String
reptm2 = do
 putStrLn "Fatal error: Put a non-negative integer!"
 m2 <- putconch "prem> "
 m2 <- if (not . and $ fmap isDigit m2)
        then reptm2
        else return m2
 return m2
reptn2 :: IO String
reptn2 = do
 putStrLn "Fatal error: Put a positive integer!"
 n2 <- putconch "exp> "
 n2 <- if (not . and $ fmap isDigit n2)
        then reptn2
        else return n2
 return n2
