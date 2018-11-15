-- The main part of the console-like environment, Welcome
module Welcome
( welcome, putconch, putcon
) where

import System.Console.Haskeline
import System.IO
import Cftdef
import Vbcftdef
import Cftegbase
import Cfteg1
-- functions import
import Fhelp
import Fexception

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
  "vbpf"    -> fc fvbpf
  otherwise -> fc fexception
-- gets input with fancy effects
putconch :: MonadException m => String -> m String
putconch = fmap dejust . runInputT defaultSettings . getInputLine
putcon :: MonadException m => m String
putcon = putconch "> "
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
