-- main
import System.IO
import System.Process
import Welcome
import Fancyput

main :: IO ()
main = do
 putStrLn "Mathverse Turning on..."
 putStrLn ""
 login 5

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
        welcome
  else do
        putStrLn ("Wrong ID or password. "++(show (x-1))++" chance(s) left.")
        login (x-1)

loginfail :: IO ()
loginfail = do
 putStrLn "Fatal error: This will be reported."
 putStrLn "Mathverse shutting down..."

ismem :: String -> String -> IO Bool
ismem x y = do
 let input = "idpwin daybreak/username" ++ " " ++ x ++ " " ++ y
 out <- readProcess "daybreak/purebreak" [] input
-- (Just hin, Just hout, _, _) <- createProcess (proc "daybreak/purebreak" []){ std_in = CreatePipe, std_out = CreatePipe }
-- hPutStr hin input
-- out <- hGetContents hout
 --let out2 = (head . tail . tail . lines) out
 --print out2
 case (out) of
  "True" -> return True
  "True\n" -> return True
  otherwise -> return False
