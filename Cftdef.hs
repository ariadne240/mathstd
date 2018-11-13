-- the most fundamental library of mathstd, Cftdef
module Cftdef
( Exp
, Wf
, Vwf
, Towf
, Pf
, Vpf
, Topf
, Cft(..)
, towf
, topf
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
class GenCft a where
 vwf :: a -> Vwf
 vpf :: a -> Vpf
data Cft = Cft { vwf :: Vwf, vpf :: Vpf }
-- vwf :: Cft -> Vwf, vpf :: Cft -> Vpf
towf :: Cft -> Towf
towf x y
 | vwf x y   = Wf (Just y)
 | otherwise = Wf Nothing
topf :: Cft -> Topf
topf x y
 | vpf x y   = Pf (Just y)
 | otherwise = Pf Nothing
