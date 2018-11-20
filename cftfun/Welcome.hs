-- The main part of the console-like environment, Welcome
module Welcome
( welcome, putconch, putcon
) where

import System.IO
import Fancyput
-- functions import
import Fhelp
import Fexception
import Fwf

welcome :: String -> IO()
welcome id = do
 putStrLn ("Dear "++id++", what do you want to do?")
 putStrLn "To get help, choose 'help'"
 w <- putcon
 case (w) of
  "end"     -> shutdown
  "help"    -> fc id fhelp
  "wf"      -> fc id fwf
  "pf"      -> fc id fpf
  "vbpf"    -> fc id fvbpf
  otherwise -> fc id fexception
-- fc for function call
fc :: String -> IO() -> IO()
fc id = (>> welcome id)
-- function shutdown
shutdown :: IO()
shutdown = do
 putStrLn ""
 putStrLn "Good bye."
 putStrLn "Mathverse Shutting down..."
 putStrLn ""
