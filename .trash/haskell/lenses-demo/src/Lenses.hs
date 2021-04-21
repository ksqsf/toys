{-# LANGUAGE RankNTypes #-}

module Lenses where

-- import Control.Lens (lens)

-- a = bigger type
-- b = smaller type
type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

data Atom = Atom { _element :: String, _point :: Point } deriving (Show, Eq)
data Point = Point { _x :: Double, _y :: Double } deriving (Show, Eq)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

-- point :: Lens' Atom Point
-- point = lens _point (\atom newPoint -> atom { _point = newPoint })

point :: Lens' Atom Point
point k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))
