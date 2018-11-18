-- main
import System.IO
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
 if (ismem id pw)
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

ismem :: String -> String -> Bool
ismem x y = case (x) of
             "root"    -> y == "root"
             "ariadne" -> y == "ariadne"
             otherwise -> False
