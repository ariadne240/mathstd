import System.Environment
import Fancyput

main :: IO ()
main = do
 putStrLn "Welcome to daybreak."
 welcome

welcome :: IO ()
welcome = do
 a <- putcon
 opch a
fc :: IO () -> IO ()
fc = (>> welcome)

opch :: String -> IO ()
opch x = do
 let op = words x
 if (length op == 0)
 then do
  putStrLn "Put a word"
  welcome
 else case (head op) of
  "end"     -> putStrLn "Shutting down daybreak..."
  "select"  -> fc $ select op
  "desc"    -> fc $ desc op
  "create"  -> fc $ create op
  "idpwin"  -> fc $ idpwin op
  otherwise -> welcome

select :: [String] -> IO ()
select x = putStrLn "select"

desc :: [String] -> IO ()
desc x = putStrLn "desc"

create :: [String] -> IO ()
create x = putStrLn "create"

idpwin :: [String] -> IO ()
idpwin x = do
 let nm = ((head . tail) x)
 let id = ((head . tail . tail) x)
 let pw = ((head . tail . tail . tail) x)
 let idpw = id ++ " " ++ pw
 y <- readFile nm
 let yc = (tail . lines) y
 print (idpw `elem` yc)
