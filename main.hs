-- main
import System.IO
import Cftdef
import Cftegbase
import Cfteg1
import Scftdef

main :: IO()
main = do
 putStrLn "Mathverse Turning on..."
 putStrLn ""
 welcome

welcome :: IO()
welcome = do
 putStrLn "What do you want to do?"
 putStrLn "Possible choices: 'wf', 'pf'"
 putStrLn "You can always shut down Mathverse with 'end'"
 w <- getLine
 case (w) of
  "end"     -> shutdown
  "wf"      -> wfmain
  "pf"      -> pfmain
  otherwise -> putStrLn "Not yet implemented"

shutdown :: IO()
shutdown = do
 putStrLn ""
 putStrLn "Good bye."
 putStrLn "Mathverse Shutting down..."

wfmain :: IO()
wfmain = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- getLine
 putStrLn "Insert your expression."
 y <- getLine
 putStrLn "Your wf:"
 print $ towf (cftch x) y
 putStrLn ""
 welcome

pfmain :: IO()
pfmain = wfmain

cftch :: String -> Vbcft
cftch "cft1" = puretovb cft1
cftch "cft2" = puretovb cft2
cftch "cft3" = puretovb cft3
cftch "cft4" = puretovb cft4
cftch "cft5" = puretovb cft5
cftch "cft6" = puretovb cft6
cftch "cft7" = cft7
cftch _      = puretovb cft1
