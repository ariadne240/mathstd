-- the definition of interpreting CFT(Itpcft), Itpcftdef
module Itpcftdef
(
-- * Interpret *

) where

import Cftdef

{--
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
