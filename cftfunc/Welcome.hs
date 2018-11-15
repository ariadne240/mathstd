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

welcome :: IO()
welcome = do
 putStrLn "What do you want to do?"
 putStrLn "To get help, choose 'help'"
 putStrLn "If you want to add your own cft, shut down this and modify the code."
-- The sentence above does not give you any rights to use, share or modify the code.
-- It is NOT about license; I, and I alone have all rights.
 w <- putcon
 case (w) of
  "end"     -> shutdown
  "help"    -> fc fhelp
  "wf"      -> fc fwf
  "pf"      -> fc fpf
  "vbpf"    -> fc fvbpf
  otherwise -> fc fexception
-- fc for function call
fc :: IO() -> IO()
fc = (>> welcome)
-- function shutdown
shutdown :: IO()
shutdown = do
 putStrLn ""
 putStrLn "Good bye."
 putStrLn "Mathverse Shutting down..."
 putStrLn ""
