-- The Fwf function, Fwf
module Fwf
( fwf, fpf, fvbpf
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
fpf :: IO()
fpf = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putconch "cft> "
 putStrLn "How many premises and to-be-proved expressions?"
 m2 <- reptm
 let m = read m2
 n2 <- reptn
 let n = read n2
 if (n <= 0)
 then do
       putStrLn "Error: You should put a positive integer."
 else return ()
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate n $ putconch "exp> ")
 let midpf2st = makepfful premises tbproved
 let midpf3st = topf23 (cftch x) midpf2st
 if (vpf (cftch x) midpf3st)
  then putStrLn "[Result] This proof is valid."
  else putStrLn "[Result] This proof is invalid."
 putStrLn ""
-- function fvbpf
fvbpf :: IO()
fvbpf = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putconch "cft> "
 putStrLn "How many premises and to-be-proved expressions?"
 m2 <- reptm
 let m = read m2
 n2 <- reptn
 let n = read n2
 if (n <= 0)
 then do
       putStrLn "Error: You should put a positive integer."
 else return ()
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate (2*n) (putconch "exp> "))
 let midvbpf3st = makevbpfful premises tbproved
 let midvbpf4st = tovbpf34 (cftch x) midvbpf3st
 if (vvbpf (cftch x) midvbpf4st)
  then putStrLn "[Result] This verbose proof is valid."
  else putStrLn "[Result] This verbose proof is invalid."
 putStrLn ""
reptm :: IO String
reptm = do
 m <- putconch "prem> "
 m <- if (not . and $ fmap isDigit m)
        then do
              putStrLn "Fatal error: Put a non-negative integer!"
              reptm
        else return m
 return m
reptn :: IO String
reptn = do
 n <- putconch "exp> "
 n <- if (not . and $ fmap isDigit n)
        then do
              putStrLn "Fatal error: Put a positive integer!"
              reptn
        else return n
 return n
