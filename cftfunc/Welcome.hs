-- The main part of the console-like environment, Welcome
module Welcome
( welcome
) where

import System.Console.Haskeline
import System.IO
import Cftdef
import Cftegbase
import Cfteg1
import Scftdef

welcome :: IO()
welcome = do
 putStrLn "What do you want to do?"
 putStrLn "To get help, choose 'help'"
 putStrLn "If you want to add your own cft, shut down this and modify the code."
-- The sentence above does not give you any rights to use, share or modify the code.
-- It is NOT about license; I, and I alone have all rights.
 w <- putcon
 case (w) of
  "help"    -> helpmain
  "end"     -> shutdown
  "wf"      -> wfmain
  "vbpf"    -> vbpfmain
  otherwise -> exceptionmain

helpmain :: IO()
helpmain = do
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
 welcome

shutdown :: IO()
shutdown = do
 putStrLn ""
 putStrLn "Good bye."
 putStrLn "Mathverse Shutting down..."
 putStrLn ""

exceptionmain :: IO()
exceptionmain = do
 putStrLn "Not yet implemented or not a valid function"
 putStrLn "Choose other options..."
 putStrLn ""
 welcome

wfmain :: IO()
wfmain = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putcon
 putStrLn "Insert your expression."
 y <- putconch "exp> "
 putStrLn "Your wf:"
 print $ towf (cftch x) y
 putStrLn ""
 welcome

--pfmain :: IO()

vbpfmain :: IO()
vbpfmain = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- putcon
 putStrLn "How many premises and to-be-proved expressions?"
 m2 <- putconch "pr> "
 let m = read m2
 n2 <- putconch "exp> "
 let n = read n2
 putStrLn "Now insert the premises."
 premises <- sequence (replicate m $ putconch "pr> ")
 putStrLn "Now insert the to-be-proved expressions."
 tbproved <- sequence (replicate (2*n) (putconch "exp> "))
 let pfstring = makepfful premises tbproved
 putStrLn ""
 welcome

putconch :: MonadException m => String -> m String
putconch = fmap dejust . runInputT defaultSettings . getInputLine
putcon :: MonadException m => m String
putcon = putconch "> "
dejust :: Maybe String -> String
dejust (Just x) = x
dejust Nothing  = ""

cftch :: String -> Vbcft
cftch "cft1" = puretovb cft1
cftch "cft2" = puretovb cft2
cftch "cft3" = puretovb cft3
cftch "cft4" = puretovb cft4
cftch "cft5" = puretovb cft5
cftch "cft6" = puretovb cft6
cftch "cft7" = cft7
cftch _      = puretovb cft1 -- default cft

makel2 :: [String] -> [(String, String)]
makel2 []        = []
makel2 [x]       = [(x, "")]
makel2 (x:x2:xs) = (x, x2):(makel2 xs)
makepfful :: [String] -> [String] -> ([String], [(String, String)])
makepfful x y = (x, makel2 y)
