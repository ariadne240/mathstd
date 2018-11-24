-- Choose cftch, Cftch
module Cftch
( cftch, setcft, getcft
) where

import Cftdef
import Vbcftdef
import Cftegbase
import Cfteg1
import Cfteg2
-- For only setcft, to be moved to other part
import System.IO
import Fancyput

cftch :: String -> Vbcft
cftch "cft1" = puretovb cft1
cftch "cft2" = puretovb cft2
cftch "cft3" = puretovb cft3
cftch "cft4" = puretovb cft4
cftch "cft5" = puretovb cft5
cftch "cft6" = puretovb cft6
cftch "cft7" = cft7
cftch "cft8" = cft8
cftch _      = puretovb cft1 -- default cft

-- how to set a cft?
setcft :: String -> String -> IO String
setcft c w = do
 let ww = words w
 if (length ww == 1)
  then do
   putStrLn ("Which cft do you want to use? (Now: "++c++")")
   putStrLn "If you choose a name which does not exist, the default cft(cft1) will be used."
   cft <- putconch "cft> "
   return cft
  else return $ (head . tail) ww

-- how to get a cft?
getcft :: String -> String -> IO ()
getcft c w = do
 putStrLn ("Now, you are using the cft "++c++".")
 putStrLn "If you want to use a different cft, use command 'setcft'."
