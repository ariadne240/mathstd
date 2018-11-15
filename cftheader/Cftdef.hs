-- the most fundamental library of mathstd, Cftdef
module Cftdef
( Exp -- the origin of everything. a piece of wanted-to-be-wf
, Oriwf, Wf, Vwf, Towf -- about wf
, Oripf, Midpf1, Midpf2, Midpf3, Pf, Vpf, Topf -- about pf
, Cft(..), Purecft(..)
, towf, topf
) where

-- The definition of CFT(Computable Formal Theory)
type Exp = String
type Oriwf = Exp
data Wf = Wf (Maybe Oriwf) deriving (Eq, Ord, Show, Read)
type Vwf = Oriwf -> Bool
type Towf = Oriwf -> Wf
type Oripf = Exp
type Midpf1 = [Exp]
type Midpf2 = ([Exp], [Exp])
type Midpf3 = ([Wf], [Wf])
data Pf = Pf (Maybe Midpf3) deriving (Eq, Ord, Show, Read)
type Vpf = Midpf3 -> Bool
type Topf = Midpf3 -> Pf
class Cft a where
 vwf :: a -> Vwf
 vpf :: a -> Vpf
data Purecft = Purecft Vwf Vpf
instance Cft Purecft where
 vwf (Purecft x y) = x
 vpf (Purecft x y) = y
towf :: Cft a => a -> Towf
towf x y
 | vwf x y   = Wf (Just y)
 | otherwise = Wf Nothing
topf :: Cft a => a -> Topf
topf x y
 | vpf x y   = Pf (Just y)
 | otherwise = Pf Nothing
