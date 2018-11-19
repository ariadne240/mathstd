import System.Environment

main :: IO ()
main = welcome

welcome :: IO ()
welcome = do
 a <- getLine
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
  "end"     -> return ()
  "select"  -> fc $ select op
  "desc"    -> fc $ desc op
  "create"  -> fc $ create op
  "idpwin"  -> idpwin op
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
 print (length x == 4 && idpw `elem` yc)
