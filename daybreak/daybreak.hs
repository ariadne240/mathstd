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
  "isrow"   -> fc $ isrow op
  otherwise -> welcome

select :: [String] -> IO ()
select x = putStrLn "select"

desc :: [String] -> IO ()
desc x = putStrLn "desc"

create :: [String] -> IO ()
create x = putStrLn "create"

isrow :: [String] -> IO ()
isrow x = do
 let lx = length x
 let nm = (head . tail) x
 y <- readFile nm
 let ly = (length . words . head . lines) y
 let yc = (tail . lines) y
 let fd = (tail . tail) x
 let tok = putspace fd
 print (lx >= 2 && lx == ly + 2 && tok `elem` yc)

putspace :: [String] -> String
putspace [] = []
putspace [x] = x
putspace (x:xs) = x ++ " " ++ putspace xs
