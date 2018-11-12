-- the definition of Structured CFT(STCF), scftdef
module Scftdef
( Vbcft(..)
, Verbosepf
, Vverbosepf
, Toverbosepf
, toverbosepf
) where

import Cftdef

data Vbcft = Vbcft { vwf :: Vwf, vpf :: Vpf, vverbosepf :: Vverbosepf }
-- vwf :: Cft -> Vwf, vpf :: Cft -> Vpf
type Cdverbosepf = ([Wf], [(Wf, String)])
data Verbosepf = Verbosepf (Maybe Cdverbosepf)
type Vverbosepf = Cdverbosepf -> Bool
type Toverbosepf = Cdverbosepf -> Verbosepf
toverbosepf :: Vbcft -> Toverbosepf
toverbosepf x y
 | vverbosepf x y = Verbosepf (Just y)
 | otherwise      = Verbosepf Nothing

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
