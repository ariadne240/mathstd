-- The Fwf function, Fwf
module Fwf
( fwf, fvbpf
) where

import System.IO
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
 x <- putcon
 putStrLn "Insert your expression."
 y <- putconch "exp> "
 putStrLn "Your wf:"
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
 x <- putcon
 putStrLn "How many premises and to-be-proved expressions?"
 m2 <- putconch "prem> "
 let m = read m2
 n2 <- putconch "exp> "
 let n = read n2
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "prem> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate (2*n) (putconch "exp> "))
 let pfstring = makepfful premises tbproved
 putStrLn ""

makel2 :: [String] -> [(Exp, String)]
makel2 []        = []
makel2 [x]       = [(x, "")]
makel2 (x:x2:xs) = (x, x2):(makel2 xs)
makepfful :: [Exp] -> [String] -> Midvbpf3
makepfful x y = (x, makel2 y)
