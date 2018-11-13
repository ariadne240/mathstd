-- the definition of Structured CFT(STCF), scftdef
module Scftdef
( Vbcft(..)
, Verbosepf
, Vverbosepf
, Toverbosepf
, toverbosepf
, cfttovbcft
, vbcfttocft
) where

import Cftdef

data Vbcft = Vbcft { vwf :: Vwf, vpf :: Vpf, vverbosepf :: Vverbosepf }
-- vwf :: Vbcft -> Vwf, vpf :: Vbcft -> Vpf, vvberosepf :: Vbcft -> Vverbosepf
type Cdverbosepf = ([Wf], [(Wf, String)])
data Verbosepf = Verbosepf (Maybe Cdverbosepf)
type Vverbosepf = Cdverbosepf -> Bool
type Toverbosepf = Cdverbosepf -> Verbosepf
toverbosepf :: Vbcft -> Toverbosepf
toverbosepf x y
 | vverbosepf x y = Verbosepf (Just y)
 | otherwise      = Verbosepf Nothing
towf :: Vbcft -> Towf
towf x y
 | vwf x y   = Wf (Just y)
 | otherwise = Wf Nothing
topf :: Vbcft -> Topf
topf x y
 | vpf x y   = Pf (Just y)
 | otherwise = Pf Nothing
cfttovbcft :: Cft -> Vbcft
cfttovbcft (Cft x y) = Vbcft x y (\x -> True)
vbcfttocft :: Vbcft -> Cft
vbcfttocft (Vbcft x y z) = Cft x y
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
