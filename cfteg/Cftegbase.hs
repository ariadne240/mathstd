-- the base of making cft examples, Cftegbase
module Cftegbase
( pfallt, pfallf
, Ttree(..), Tst
, transtree, treechk
) where

import Cftdef

-- When only dealing with wfs, not pfs.
pfallt :: Vwf -> Purecft
pfallt u = Purecft u (\x -> True)
pfallf :: Vwf -> Purecft
pfallf u = Purecft u (\x -> False)
-- Ttree for transformed tree
data Ttree a = Tnd a | Tbr { getTtree :: [Ttree a] } deriving (Eq, Ord, Show, Read)
type Tst = Ttree String
-- proc for processing: l for layer, wd for word, bt for building tree
proc :: Char -> (Int, String, Tst) -> Maybe (Int, String, Tst)
proc c (l, wd, bt)
 | c == ' '  = Just (l, [], btadd wd l bt)
 | c == ')'  = Just (l+1, [], btset l bt)
 | c == '('  = case (l) of
                0 -> Nothing
                _ -> Just (l-1, [], btadd wd l bt)
 -- takes some care of special characters...
 | c == '~'  = Just (l, wd, bt) -- Special character '~', ignore it!
 | c == '`'  = Just (l, c:wd, bt) -- Special character '`', for ` method
 | c == '!'  = Just (l, wd, bt) -- Special character '!', ignore it!
 | c == '@'  = Just (l, wd, bt) -- Special character '@', ignore it!
 | c == '#'  = Just (l, wd, bt) -- Special character '#', ignore it!
 | c == '$'  = Just (l, c:wd, bt) -- Special character '$', for $ method
 | c == '%'  = Just (l, wd, bt) -- Special character '%', ignore it!
 | c == '^'  = Just (l, wd, bt) -- Special character '^', ignore it!
 | c == '&'  = Just (l, wd, bt) -- Special character '&', ignore it!
 | c == '*'  = Just (l, wd, bt) -- Special character '*', ignore it!
 | c == '-'  = Just (l, wd, bt) -- Special character '-', ignore it!
 | c == '+'  = Just (l, wd, bt) -- Special character '+', ignore it!
 | c == '='  = Just (l, wd, bt) -- Special character '=', ignore it!
 | c == '|'  = Just (l, wd, bt) -- Special character '|', ignore it!
 | c == '}'  = Just (l, wd, bt) -- Special character '}', ignore it!
 | c == '{'  = Just (l, wd, bt) -- Special character '{', ignore it!
 | c == ']'  = Just (l, wd, bt) -- Special character ']', ignore it!
 | c == '['  = Just (l, wd, bt) -- Special character '[', ignore it!
 | c == ':'  = Just (l, wd, bt) -- Special character ':', ignore it!
 | c == ';'  = Just (l, wd, bt) -- Special character ';', ignore it!
 | c == '>'  = Just (l, wd, bt) -- Special character '>', ignore it!
 | c == '<'  = Just (l, wd, bt) -- Special character '<', ignore it!
 | c == '.'  = Just (l, wd, bt) -- Special character '.', ignore it!
 | c == ','  = Just (l, wd, bt) -- Special character ',', ignore it!
 | c == '?'  = Just (l, wd, bt) -- Special character '?', ignore it!
 | c == '/'  = Just (l, wd, bt) -- Special character '/', ignore it!
 | normch c  = Just (l, c:wd, bt)
 | otherwise = Just (l, wd, bt) -- Other special characters, ignore it!
-- Do not use backslash('\') which might raise confusions
-- A~Z, a~z, 0~9, _
normch :: Char -> Bool
normch x = x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
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
transtree = (>>= (return . chkfn2 . tuthird)) . (>>= chkfn1) . foldr ((=<<) . proc) (return (0, [], Tbr []))
-- gets the third element of the tuple
tuthird :: (a, b, c) -> c
tuthird (_, _, x) = x
-- check finally before going into tuthird
chkfn1 :: (Int, String, Tst) -> Maybe (Int, String, Tst)
chkfn1 (l, wd, bt)
 | l == 0    = Just (0, [], btadd wd l bt)
 | otherwise = Nothing
treechk :: (Tst -> Bool) -> Maybe Tst -> Bool
treechk _ Nothing  = False
treechk x (Just y) = x y
-- check after going into tuthird
chkfn2 :: Tst -> Tst
chkfn2 (Tbr [Tnd y]) = Tnd y
chkfn2 x  = x 
