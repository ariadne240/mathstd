-- the definition of CFT(Computable Formal Theory), Cftdef
module Cftdef
(
-- * Basic *
-- a piece of wanted-to-be-wf
  Exp
-- a process to make wf
, Oriwf, Wf
-- verify and make wf
, Vwf, Towf
-- a process to make pf
, Oripf, Midpf1, Midpf2, Midpf3, Pf
-- verify and make pf
, Vpf, Topf
-- a typeclass for general cfts
, Cft(..)
-- a type for pure cft without any additional structures
, Purecft(..)
-- the function to extract towf info from Cft
, towf
-- the function to extract topf info from Cft
, topf
) where

-- The definition of Exp
type Exp = String
-- The definition of Wf
type Oriwf = Exp
data Wf = Wf (Maybe Oriwf) deriving (Eq, Ord, Show, Read)
type Vwf = Oriwf -> Bool
type Towf = Oriwf -> Wf
-- The definition of Pf
type Oripf = Exp
type Midpf1 = [Exp]
type Midpf2 = ([Exp], [Exp])
type Midpf3 = ([Wf], [Wf])
data Pf = Pf (Maybe Midpf3) deriving (Eq, Ord, Show, Read)
type Vpf = Midpf3 -> Bool
type Topf = Midpf3 -> Pf
-- The definition of typeclass Cft
class Cft a where
 vwf :: a -> Vwf
 vpf :: a -> Vpf
-- The definition of type Purecft and related typeclasses
data Purecft = Purecft Vwf Vpf
instance Cft Purecft where
 vwf (Purecft x y) = x
 vpf (Purecft x y) = y
-- The definition of the function to extract towf info from Cft
towf :: Cft a => a -> Towf
towf x y
 | vwf x y   = Wf (Just y)
 | otherwise = Wf Nothing
-- The definition of the function to extract topf info from Cft
topf :: Cft a => a -> Topf
topf x y
 | vpf x y   = Pf (Just y)
 | otherwise = Pf Nothing

-- topf01 :: Cft a => a -> Oripf -> Midpf1 -- Not useful
-- topf01 _ = lines
-- topf12 :: Cft a => a -> Midpf1 -> Midpf2 -- Impossible
-- topf12 _
-- topf23 :: Cft a => a -> Midpf2 -> Midpf3
-- topf23 x
