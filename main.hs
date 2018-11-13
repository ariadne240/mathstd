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

cftch :: Int -> Vbcft
cftch 1 = cfttovbcft cft1
cftch 2 = cfttovbcft cft2
cftch 3 = cfttovbcft cft3
cftch 4 = cfttovbcft cft4
cftch 5 = cfttovbcft cft5
cftch 6 = cfttovbcft cft6
cftch 7 = cft7
cftch _ = cfttovbcft cft1
