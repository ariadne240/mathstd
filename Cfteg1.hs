-- the rigid examples of cfts, Cfteg1
module Cfteg1
( cft1
, cft2
, cft3
, cft4
, cft5
, cft6
) where

import Cftdef
import Cftegbase

-- Elementary cfts
cft1 :: Cft
cft1 = pfallt (\x -> True)
cft2 :: Cft
cft2 = pfallf (\x -> True)
cft3 :: Cft
cft3 = pfallf (\x -> False)
-- Using some trees
cft4 :: Cft
cft4 = pfallt cft4wf
cft4wf :: Vwf
cft4wf = cft4tr . transtree
cft4tr :: Maybe Tst -> Bool
cft4tr Nothing  = False
cft4tr (Just y) = True
-- Reallife cft
cft5 :: Cft
cft5 = pfallt cft5wf
cft5wf :: Vwf
cft5wf = cft5tr . transtree
cft5tr :: Maybe Tst -> Bool
cft5tr Nothing  = False
cft5tr (Just x) = cft5trr ['f', 'g', 'h'] x
cft5trr :: String -> Tst -> Bool
cft5trr ls (Tnd [])     = False
cft5trr ls (Tnd (x:xs)) = x `elem` ls
cft5trr ls (Tbr y)      = and $ fmap (cft5trr ls) y
-- Reallife cft2
cft6 :: Cft
cft6 = pfallt cft6wf
cft6wf :: Vwf
cft6wf = cft6tr . transtree
cft6tr :: Maybe Tst -> Bool
cft6tr Nothing  = False
cft6tr (Just x) = cft6trr ['x', 'y', 'z'] x
cft6trr :: [Char] -> Tst -> Bool
cft6trr ls (Tbr [])     = False
cft6trr ls (Tbr (y:ys)) = case (y) of
                        (Tnd z) -> if (length z > 0 && (head z) `elem` ls)
                                   then True
                                   else case (z) of
                                         "fnot" -> length ys == 1 && and (fmap (cft6trr ls) ys)
                                         "fand" -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                         "for"  -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                         _      -> False
                        (Tbr _) -> False
cft6trr ls (Tnd [])     = False
cft6trr ls (Tnd (x:xs)) = x `elem` ls
