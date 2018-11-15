-- the most fundamental library of mathstd, Cftdef
module Cftdef
( Exp -- a piece of wanted-to-be-wf
-- a process to make wf
, Oriwf, Wf, Vwf, Towf
-- a process to make pf
, Oripf, Midpf1, Midpf2, Midpf3, Pf, Vpf, Topf
, Cft(..) -- a typeclass for general cfts
, Purecft(..) -- a type for pure cft without any additional structures
-- What is my towf of cft
, towf
-- what is my topf of cft
, topf
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
