-- the rigid examples of cfts, Cfteg2
module Cfteg2
( cft4, cft5, cft6 -- Purecft
, cft7, cft8 -- Vbcft
) where

import Cftdef
import Cftegbase
import Vbcftdef
import Data.List (inits)
import Data.Char

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
                        (Tnd z) -> case (z) of
                                    "ft"   -> length ys == 0
                                    "ff"   -> length ys == 0
                                    "fnot" -> length ys == 1 && and (fmap (cft6trr ls) ys)
                                    "fand" -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                    "for"  -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                    "fif"  -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                    "fiff" -> length ys == 2 && and (fmap (cft6trr ls) ys)
                                    _      -> False
                        (Tbr _) -> False
cft6trr ls (Tnd x)      = case (x) of
                        []      -> False
                        (xh:xs) -> xh `elem` ls || x `elem` ["ft", "ff"]
-- Reallife Vbcft
cft7 :: Vbcft
cft7 = Vbcft cft6wf cft7pf cft7vpf
cft7pf :: Vpf
cft7pf = unverbosify cft7vpf
cft7vpf :: Vvbpf
cft7vpf = (\x -> True)
-- Reallife Vbcft2: Hilbert System
cft8 :: Vbcft
cft8 = Vbcft cft6wf cft8pf cft8vpf
cft8pf :: Vpf
cft8pf = unverbosify cft8vpf
cft8vpf :: Vvbpf
cft8vpf (x, y) = if (chkwfl x && chkwfl (fmap fst y))
                  then (vlongpf . fmap twftotree . longpf) (x, y)
                  else False
vlongpf :: [(Tst, String)] -> Bool
vlongpf = and . fmap vspec . tail . inits
vspec :: [(Tst, String)] -> Bool
vspec = vspec8 <$> fmap fst . init <*> fst . last <*> snd . last
vspec8 :: [Tst] -> Tst -> String -> Bool
vspec8 x y z
 | hd == "premise" = True
 | hd == "axiom"   = ll == 0 && axiom8 y
 | hd == "rmp"     = ll == 2 && mp8 tl x y
 | otherwise       = False
 where
  hd = (head . words) z
  tl = (tail . words) z
  ll = length tl
axiom8 :: Tst -> Bool
axiom8 = challlist [axiom81, axiom82, axiom83, axiom84]
axiom81 :: Tst -> Bool
axiom81 = (== Tnd "ft")
axiom82 :: Tst -> Bool
axiom82 x = case (x) of
             (Tbr [Tnd "fif", y, Tbr [Tnd "fif", z, w]]) -> y == w
             otherwise                                   -> False
axiom83 :: Tst -> Bool
axiom83 x = case (x) of
             (Tbr [Tnd "fif", Tbr [Tnd "fif", x1, Tbr [Tnd "fif", y1, z1]], Tbr [Tnd "fif", Tbr [Tnd "fif", x2, y2], Tbr [Tnd "fif", x3, z2]]]) -> x1 == x2 && x2 == x3 && y1 == y2 && z1 == z2
             otherwise -> False
axiom84 :: Tst -> Bool
axiom84 x = case (x) of
             (Tbr [Tnd "fif", Tbr [Tnd "fif", Tbr [Tnd "fnot", x1], Tbr [Tnd "fnot", y1]], Tbr [Tnd "fif", y2, x2]]) -> x1 == x2 && y1 == y2
             otherwise -> False
mp8 :: [String] -> [Tst] -> Tst -> Bool
mp8 tl x y
 | and (fmap isDigit $ head tl) && and (fmap isDigit $ last tl) = 0 < wfn1 && 0 < wfn2 && wfn1 <= lx && wfn2 <= lx && (x !! (wfn2-1)) == Tbr [Tnd "fif", (x !! (wfn1-1)), y]
 | otherwise = False
 where
  wfn1 = read $ head tl
  wfn2 = read $ last tl
  lx = length x
-- (Wf, String) to (Tst, String)
twftotree :: (Wf, String) -> (Tst, String)
twftotree (x, y) = (wftotree x, y)
wftotree :: Wf -> Tst
wftotree = dejusttr . transtree . dewf
dejusttr :: Maybe Tst -> Tst
dejusttr (Just x) = x
dejusttr Nothing  = Tbr []
-- check a list of wf whether every element is NOT Nothing
chkwfl :: [Wf] -> Bool
chkwfl [] = True
chkwfl (x:xs) = if (x == Wf Nothing)
                 then False
                 else chkwfl xs
-- check every conditions in a list and 'or' it
challlist :: [a -> Bool] -> a -> Bool
challlist x = or . (x <*>) . pure
