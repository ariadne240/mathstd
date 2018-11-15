-- the definition of verbose CFT(Vbcft), Vbcftdef
module Vbcftdef
(
-- * Verbose *
-- a process to make verbosepf
  Oriverbosepf, Midverbosepf1, Midverbosepf2, Cdverbosepf, Verbosepf
-- verify and make verbosepf
, Vverbosepf, Toverbosepf
-- a typeclass for cft with verboseness
, Verbose(..)
-- a class for cft with additional information of verbosepf
, Vbcft(..)
-- the function to extract toverbosewf info from Vbcft
, toverbosepf
-- * Compatibility *
-- Verbose pf to Pf, Pf to Verbose pf
, unverbosify, verbosify
-- Purecft to Vbcft, Vbcft to Purecft
, puretovb, vbtopure
) where

import Cftdef

-- The definition of Verbosepf
type Oriverbosepf = String
type Midverbosepf1 = [String]
type Midverbosepf2 = ([Exp], [String])
type Cdverbosepf = ([Wf], [(Wf, String)])
data Verbosepf = Verbosepf (Maybe Cdverbosepf) deriving (Eq, Ord, Show, Read)
type Vverbosepf = Cdverbosepf -> Bool
type Toverbosepf = Cdverbosepf -> Verbosepf
-- The definition of typeclass Verbose
class Verbose a where
 vverbosepf :: a -> Vverbosepf
-- The definition of type Vbcft and related typeclasses
data Vbcft = Vbcft Vwf Vpf Vverbosepf
instance Cft Vbcft where
 vwf (Vbcft x y _) = x
 vpf (Vbcft x y _) = y
instance Verbose Vbcft where
 vverbosepf (Vbcft _ _ x) = x
-- The definition of the function to extract toverbosewf info from Vbcft
toverbosepf :: Vbcft -> Toverbosepf
toverbosepf x y
 | vverbosepf x y = Verbosepf (Just y)
 | otherwise      = Verbosepf Nothing
-- Compatibility
unverbosify :: Vverbosepf -> Vpf
unverbosify = (. tovbtemp)
verbosify :: Vpf -> Vverbosepf
verbosify = (. toprtemp)
tovbtemp :: Midpf3 -> Cdverbosepf
tovbtemp (x, y) = (x, fmap (\z -> (z, "")) y) 
toprtemp :: Cdverbosepf -> Midpf3
toprtemp (x, y) = (x, fmap fst y)
puretovb :: Purecft -> Vbcft
puretovb (Purecft x y) = Vbcft x y (verbosify y)
vbtopure :: Vbcft -> Purecft
vbtopure (Vbcft x y z) = Purecft x y
