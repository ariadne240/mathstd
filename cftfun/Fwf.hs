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
fwf :: String -> IO()
fwf x = do
 let xc = cftch x
 putStrLn "Insert your expression."
 y <- putconch "exp> "
 putStrLn "[Result] Your wf:"
 print $ towf xc y
 putStrLn ""
-- function fpf
fpf :: String -> IO()
fpf x = do
 let xc = cftch x
 putStrLn "How many premises and to-be-proved expressions?"
 m <- repm
 n <- repn
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate n $ putconch "exp> ")
 let midpf2st = makepfful premises tbproved
 let midpf3st = topf23 xc midpf2st
 if (vpf xc midpf3st)
  then putStrLn "[Result] This proof is valid."
  else putStrLn "[Result] This proof is invalid."
 putStrLn ""
-- function fvbpf
fvbpf :: String -> IO()
fvbpf x = do
 let xc = cftch x
 putStrLn "How many premises and to-be-proved expressions?"
 m <- repm
 n <- repn
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate (2*n) (putconch "exp> "))
 let midvbpf3st = makevbpfful premises tbproved
 let midvbpf4st = tovbpf34 xc midvbpf3st
 if (vvbpf xc midvbpf4st)
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
