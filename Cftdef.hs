-- the most fundamental library of mathstd, Cftdef
module Cftdef
( Exp, Wf, Vwf, Towf, Pf, Vpf, Topf
, Cft(..), Purecft(..)
, towf, topf
) where

-- The definition of CFT(Computable Formal Theory)
type Exp = String
type Cdwf = Exp
data Wf = Wf (Maybe Cdwf) deriving (Eq, Ord, Show, Read)
type Vwf = Cdwf -> Bool
type Towf = Cdwf -> Wf
type Cdpf = ([Wf], [Wf])
data Pf = Pf (Maybe Cdpf) deriving (Eq, Ord, Show, Read)
type Vpf = Cdpf -> Bool
type Topf = Cdpf -> Pf
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
