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
cftch 1 = puretovb cft1
cftch 2 = puretovb cft2
cftch 3 = puretovb cft3
cftch 4 = puretovb cft4
cftch 5 = puretovb cft5
cftch 6 = puretovb cft6
cftch 7 = cft7
cftch _ = puretovb cft1
