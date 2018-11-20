-- main
import System.IO
import System.Process
import Fancyput
-- functions import
import Fhelp
import Fexception
import Fwf
import Cftch

main :: IO ()
main = do
 putStrLn "Mathverse Turning on..."
 putStrLn ""
 license
 putStrLn ""
 login loginav

login :: Int -> IO ()
login 0 = loginfail
login x = do
 putStrLn "Log in:"
 id <- putconch "ID> "
 pw <- putconch "PW> "
 mem <- ismem id pw
 if (mem)
  then do
        putStrLn ("Logging in with the account '"++id++"'...")
        putStrLn ""
        welcome id "cft1"
  else do
        putStrLn ("Wrong ID or password. "++(show (x-1))++" chance(s) left.")
        login (x-1)

loginfail :: IO ()
loginfail = do
 putStrLn "Fatal error: This will be reported."
 putStrLn "Mathverse shutting down..."

ismem :: String -> String -> IO Bool
ismem x y = do
 let input = "isrow daybreak/username" ++ " " ++ x ++ " " ++ y
 (Just hin, Just hout, _, ph) <- createProcess (proc "daybreak/daybreak" []){ std_in = CreatePipe, std_out = CreatePipe }
{--
It is an issue that the 'cwd' in createProcess works environment-dependently...
I did not intend to use an absolute path so I used this:
let input = "idpwin daybreak/username" ++ " " ++ x ++ " " ++ y
(Just hin, Just hout, _, ph) <- createProcess (proc "daybreak/daybreak" []){ std_in = CreatePipe, std_out = CreatePipe }
instead of this:
let input = "idpwin username" ++ " " ++ x ++ " " ++ y
(Just hin, Just hout, _, ph) <- createProcess (proc "daybreak" []){ cwd = Just "daybreak", std_in = CreatePipe, std_out = CreatePipe }
--}
 hPutStr hin input
 hClose hin
 out <- hGetLine hout
 out2 <- hGetLine hout
 hClose hout
-- waitForProcess ph
 case (out2) of
  "True" -> return True
  "True\n" -> return True
  "> True" -> return True
  "> True\n" -> return True
  otherwise -> return False

welcome :: String -> String -> IO()
welcome id c = do
 putStrLn ("Dear "++id++", what do you want to do?")
 putStrLn "To get help, choose 'help'"
 w <- putcon
 case (w) of
  "end"     -> shutdown id
  "help"    -> fc id c fhelp
  "logout"  -> logout id
  "setcft"  -> setcft c >>= welcome id
  "wf"      -> fc id c (fwf c)
  "pf"      -> fc id c (fpf c)
  "vbpf"    -> fc id c (fvbpf c)
  otherwise -> fc id c fexception
-- fc for function call
fc :: String -> String -> IO() -> IO()
fc id c = (>> welcome id c)
-- function shutdown
shutdown :: String -> IO()
shutdown id = do
 putStrLn ""
 putStrLn ("Good bye, "++id++".")
 putStrLn "Mathverse Shutting down..."
 putStrLn ""
-- function logout
logout :: String -> IO()
logout id = do
 putStrLn ""
 putStrLn ("Good bye, "++id++".")
 login loginav
-- how much login available?
loginav :: Int
loginav = 5






license :: IO ()
license = do
 contents <- readFile "license"
 putStr contents
