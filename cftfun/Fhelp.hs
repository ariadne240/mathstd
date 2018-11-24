-- The Help function, Fhelp
module Fhelp
( fhelp
) where

import System.IO

fhelp :: IO()
fhelp = readFile "help" >>= putStr >> putStrLn ""
