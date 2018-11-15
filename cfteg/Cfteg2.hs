-- the rigid examples of cfts, Cfteg2
module Cfteg2
( cft4, cft5, cft6 -- Purecft
, cft7 -- Vbcft
) where

import Cftdef
import Cftegbase
import Vbcftdef

-- Using some trees
cft4 :: Purecft
cft4 = pfallt cft4wf
cft4wf :: Vwf
cft4wf = cft4tr . transtree
cft4tr :: Maybe Tst -> Bool
cft4tr = treechk (\x -> True)
-- Cft which checks tree structure
cft5 :: Purecft
cft5 = pfallt cft5wf
cft5wf :: Vwf
cft5wf = cft5tr . transtree
cft5tr :: Maybe Tst -> Bool
cft5tr = treechk (cft5trr ['f', 'g', 'h'])
cft5trr :: String -> Tst -> Bool
cft5trr ls (Tnd [])     = False
cft5trr ls (Tnd (x:xs)) = x `elem` ls
cft5trr ls (Tbr y)      = and $ fmap (cft5trr ls) y
-- More cft which checks tree structure
cft6 :: Purecft
cft6 = pfallt cft6wf
cft6wf :: Vwf
cft6wf = cft6tr . transtree
cft6tr :: Maybe Tst -> Bool
cft6tr = treechk (cft6trr ['x', 'y', 'z'])
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
-- Reallife Vbcft
cft7 :: Vbcft
cft7 = Vbcft cft6wf cft7pf cft7vpf
cft7pf :: Vpf
cft7pf = unverbosify cft7vpf
cft7vpf :: Vvbpf
cft7vpf = (\x -> True)
cft7vppf :: ([String], [(String, String)]) -> Midvbpf4
cft7vppf x = ([], [])