-- the base of making cft examples, Cftegbase
module Cftegbase
( pfallt
, pfallf
, Tst(..)
, transtree
) where

import Cftdef

-- When only dealing with wfs, not pfs.
pfallt :: Vwf -> Cft
pfallt u = Cft u (\x -> True)
pfallf :: Vwf -> Cft
pfallf u = Cft u (\x -> False)

-- Ttree for transformed tree
-- data Ttree a = Tnd a | Tbr { getTtree :: [Ttree a] } deriving (Show)
-- data Tst = Ttree String
-- But error occurred.... :(
data Tst = Tnd String | Tbr { getTtree :: [Tst] } deriving (Eq, Ord, Show, Read)
-- proc for processing (Still needs fixing)
-- l for layer, wd for word, bt for building tree
-- Use "Sorry (I can (only afford to) lose) (one friend) today"
proc :: Char -> (Int, String, Tst) -> Maybe (Int, String, Tst)
proc c (l, wd, bt)
 | c == ' '  = Just (l, [], btadd wd l bt)
 | c == ')'  = Just (l+1, [], btset l bt)
 | c == '('  = case (l) of
                0 -> Nothing
                _ -> Just (l-1, [], btadd wd l bt)
 | c == '!'  = Just (l, wd, bt) -- Special character '!', ignore it!
 | otherwise = Just (l, c:wd, bt)
-- set bt (add Tbr [] in bt)
btset :: Int -> Tst -> Tst
btset 0 = Tbr . ((Tbr []):) . getTtree
btset l = Tbr . ((:) <$> btset (l-1) . head <*> tail) . getTtree
-- add fresh Tnd in bt
btadd :: String -> Int -> Tst -> Tst
btadd [] _ = id
btadd wd 0 = Tbr . ((Tnd wd):) . getTtree
btadd wd l = Tbr . ((:) <$> btadd wd (l-1) . head <*> tail) . getTtree
-- Change String to Maybe Tst (main function!)
transtree :: String -> Maybe Tst
transtree = (>>= (return . tuthird)) . (>>= chkfn) . foldr ((=<<) . proc) (return (0, [], Tbr []))
-- gets the third element of the tuple
tuthird :: (a, b, c) -> c
tuthird (_, _, x) = x
-- check finally before going into tuthird
chkfn :: (Int, String, Tst) -> Maybe (Int, String, Tst)
chkfn (l, wd, bt)
 | l == 0    = Just (0, [], btadd wd l bt)
 | otherwise = Nothing
