-- the definition of verbose CFT(Vbcft), Vbcftdef
module Vbcftdef
(
-- * Verbose *
-- a process to make vbpf
  Orivbpf, Midvbpf1, Midvbpf2, Midvbpf3, Midvbpf4, Vbpf(..)
-- verify and make vbpf
, Vvbpf, Tovbpf
-- a typeclass for cft with verboseness
, Vb(..)
-- a class for cft with additional information of vbpf
, Vbcft(..)
-- the function to extract tovbpf info from Vbcft
, tovbpf, tovbpf34
-- * Compatibility *
-- Verbose pf to Pf, Pf to Verbose pf
, unverbosify, verbosify
-- Purecft to Vbcft, Vbcft to Purecft
, puretovb, vbtopure
-- Help to transfrom exp to wf
, makevbpfful
) where

import Cftdef

-- The definition of Vbpf
type Orivbpf = String
type Midvbpf1 = [String]
type Midvbpf2 = ([Exp], [String])
type Midvbpf3 = ([Exp], [(Exp, String)])
type Midvbpf4 = ([Wf], [(Wf, String)])
data Vbpf = Vbpf (Maybe Midvbpf4) deriving (Eq, Ord, Show, Read)
type Vvbpf = Midvbpf4 -> Bool
type Tovbpf = Midvbpf4 -> Vbpf
-- The definition of typeclass Vb
class Vb a where
 vvbpf :: a -> Vvbpf
-- The definition of type Vbcft and related typeclasses
data Vbcft = Vbcft Vwf Vpf Vvbpf
instance Cft Vbcft where
 vwf (Vbcft x y _) = x
 vpf (Vbcft x y _) = y
instance Vb Vbcft where
 vvbpf (Vbcft _ _ x) = x
-- The definition of the function to extract tovbpf info from Vbcft
tovbpf :: Vbcft -> Tovbpf
tovbpf x y
 | vvbpf x y = Vbpf (Just y)
 | otherwise = Vbpf Nothing
tovbpf34 :: Cft a => a -> Midvbpf3 -> Midvbpf4
tovbpf34 c (x, y) = (fmap (towf c) x, fmap (\(w, z) -> (towf c w, z)) y)
-- * Compatibility *
-- Verbose pf to Pf, Pf to Verbose pf
unverbosify :: Vvbpf -> Vpf
unverbosify = (. tovbtemp)
verbosify :: Vpf -> Vvbpf
verbosify = (. toprtemp)
tovbtemp :: Midpf3 -> Midvbpf4
tovbtemp (x, y) = (x, fmap (\z -> (z, "")) y) 
toprtemp :: Midvbpf4 -> Midpf3
toprtemp (x, y) = (x, fmap fst y)
-- Purecft to Vbcft, Vbcft to Purecft
puretovb :: Purecft -> Vbcft
puretovb (Purecft x y) = Vbcft x y (verbosify y)
vbtopure :: Vbcft -> Purecft
vbtopure (Vbcft x y z) = Purecft x y
-- Help to transfrom exp to wf
makel2 :: [String] -> [(Exp, String)]
makel2 []        = []
makel2 [x]       = [(x, "")]
makel2 (x:x2:xs) = (x, x2):(makel2 xs)
makevbpfful :: [Exp] -> [String] -> Midvbpf3
makevbpfful x y = (x, makel2 y)
