-- the rigid examples of cfts, Cfteg1
module Cfteg1
( cft1, cft2, cft3 -- Purecft
) where

import Cftdef
import Cftegbase
import Vbcftdef

-- Elementary cfts
cft1 :: Purecft
cft1 = pfallt (\x -> True)
cft2 :: Purecft
cft2 = pfallf (\x -> True)
cft3 :: Purecft
cft3 = pfallf (\x -> False)
