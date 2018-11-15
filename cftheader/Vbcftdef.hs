-- the definition of verbose CFT(VBCFT), vbcftdef
module Vbcftdef
(
-- * Verbose *
-- a process to make verbosepf
  Oriverbosepf, Midverbosepf1, Midverbosepf2, Cdverbosepf, Verbosepf, Vverbosepf, Toverbosepf
, Verbose(..) -- a typeclass for cft with verboseness
, Vbcft(..) -- a class for cft with additional information of verbosepf
-- what is my toverbosepf of vbcft
, toverbosepf
, unverbosify, verbosify -- Verbose pf to Pf, Pf to Verbose pf
, puretovb, vbtopure -- Purecft to Vbcft, Vbcft to Purecft

-- * Interpret *
) where

import Cftdef

type Oriverbosepf = String
type Midverbosepf1 = [String]
type Midverbosepf2 = ([Exp], [String])
type Cdverbosepf = ([Wf], [(Wf, String)])
data Verbosepf = Verbosepf (Maybe Cdverbosepf) deriving (Eq, Ord, Show, Read)
type Vverbosepf = Cdverbosepf -> Bool
type Toverbosepf = Cdverbosepf -> Verbosepf

class Verbose a where
 vverbosepf :: a -> Vverbosepf
data Vbcft = Vbcft Vwf Vpf Vverbosepf
instance Cft Vbcft where
 vwf (Vbcft x y _) = x
 vpf (Vbcft x y _) = y
instance Verbose Vbcft where
 vverbosepf (Vbcft _ _ x) = x

toverbosepf :: Vbcft -> Toverbosepf
toverbosepf x y
 | vverbosepf x y = Verbosepf (Just y)
 | otherwise      = Verbosepf Nothing
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
{--
-- Interpret
type Cdinterpretable = Exp
data Interpretable = Interpretable (Maybe Cdinterpretable)
type Vinterpretable = Cdinterpretable -> Bool
type Tointerpretable = Cdinterpretable -> Interpretable
tointerpretable :: Stcft -> Tointerpretable
tointerpretable x y
 | interpretable x y = Interpretable (Just y)
 | otherwise = Interpretable Nothing
interpret :: Interpretable -> Wf
-- Prosperous CFT(PCFT) : CFT with structures rich enough
--}

-- To be revisited...
