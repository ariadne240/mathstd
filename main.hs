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
 putStrLn "Possible choices: 'wf', 'pf', 'vbpf'"
 putStrLn "You can always shut down Mathverse with 'end'"
 putStrLn "If you want to add your own cft, shut down this and modify the code."
-- The sentence above does not give you any rights to use, share or modify the code.
-- It is NOT about license; I, and I alone have all rights.
 w <- getLine
 case (w) of
  "end"     -> shutdown
  "wf"      -> wfmain
  "vbpf"    -> vbpfmain
  otherwise -> exceptionmain

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
 x <- getLine
 putStrLn "Insert your expression."
 y <- getLine
 putStrLn "Your wf:"
 print $ towf (cftch x) y
 putStrLn ""
 welcome

vbpfmain :: IO()
vbpfmain = do
 putStrLn "Which cft do you want to use?"
 putStrLn "If you choose a name which does not exist, the default cft will be used."
 putStrLn "(The first character of cfts is always lower-case)"
 x <- getLine
 putStrLn "How many premises and to-be-proved expressions?"
 m <- readLn
 n <- readLn
 premises <- sequence (replicate m getLine)
 tbproved <- sequence (replicate (2*n) getLine)
 let pfstring = makepfful premises tbproved
 putStrLn ""
 welcome

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
