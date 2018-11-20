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
 x <- setcft
 putStrLn "Insert your expression."
 y <- putconch "exp> "
 putStrLn "[Result] Your wf:"
 print $ towf (cftch x) y
 putStrLn ""
-- function fpf
fpf :: IO()
fpf = do
 x <- setcft
 putStrLn "How many premises and to-be-proved expressions?"
 m <- repm
 n <- repn
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
 x <- setcft
 putStrLn "How many premises and to-be-proved expressions?"
 m <- repm
 n <- repn
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
repm :: IO Int
repm = do
 m <- putconch "prem> "
 m2 <- if (not . and $ fmap isDigit m)
        then do
              putStrLn "Fatal error: Put a non-negative integer!"
              repm
        else return $ read m
 return m2
repn :: IO Int
repn = do
 n <- putconch "exp> "
 n2 <- if ((not . and $ fmap isDigit n) || read n <= 0)
        then do
              putStrLn "Fatal error: Put a positive integer!"
              repn
        else return $ read n
 return n2
