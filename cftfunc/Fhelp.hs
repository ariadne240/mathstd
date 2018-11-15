-- The Help function, Fhelp
module Fhelp
( fhelp
) where

import System.IO

fhelp :: IO()
fhelp = do
 putStrLn "* Help *"
 putStrLn "E"
 putStrLn "end: shut down Mathverse"
 putStrLn "H"
 putStrLn "help: get help"
 putStrLn "V"
 putStrLn "vbpf: check verbose pf"
 putStrLn "W"
 putStrLn "wf: check wf"
 putStrLn ""
