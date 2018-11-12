-- main
import System.IO
import Cftdef
import Cftegbase
import Cfteg1
import Scftdef

main :: IO()
main = do
 print "Which cft do you want to use?"
 n <- readLn
 print "Insert your expression."
 x <- getLine
 print "Your wf:"
 print $ towf (cftch n) x

cftch :: Int -> Cft
cftch 1 = cft1
cftch 2 = cft2
cftch 3 = cft3
cftch 4 = cft4
cftch 5 = cft5
cftch 6 = cft6
cftch _ = cft1
