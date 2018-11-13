-- the definition of Structured CFT(STCF), scftdef
module Scftdef
(
-- Verbose part
  Verbose(..), Vbcft(..), Verbosepf, Vverbosepf, Toverbosepf
, toverbosepf
, unverbosify, verbosify -- Verbose pf to Pf, Pf to Verbose pf
, puretovb, vbtopure -- Purecft to Vbcft, Vbcft to Purecft
) where

import Cftdef

data Vbcft = Vbcft Vwf Vpf Vverbosepf
class Verbose a where
 vverbosepf :: a -> Vverbosepf
instance Cft Vbcft where
 vwf (Vbcft x y _) = x
 vpf (Vbcft x y _) = y
instance Verbose Vbcft where
 vverbosepf (Vbcft _ _ x) = x
type Cdverbosepf = ([Wf], [(Wf, String)])
data Verbosepf = Verbosepf (Maybe Cdverbosepf)
type Vverbosepf = Cdverbosepf -> Bool
type Toverbosepf = Cdverbosepf -> Verbosepf
toverbosepf :: Vbcft -> Toverbosepf
toverbosepf x y
 | vverbosepf x y = Verbosepf (Just y)
 | otherwise      = Verbosepf Nothing
unverbosify :: Vverbosepf -> Vpf
unverbosify x = (\x -> True)
verbosify :: Vpf -> Vverbosepf
verbosify x = (\x -> True)
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
