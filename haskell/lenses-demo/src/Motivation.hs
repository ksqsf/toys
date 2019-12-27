{-# LANGUAGE TemplateHaskell #-}

module Motivation where

import Control.Lens hiding (element)
import Control.Lens.TH

data Atom = Atom { _element :: String, _point :: Point } deriving (Show, Eq)
data Point = Point { _x :: Double, _y :: Double } deriving (Show, Eq)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

atom :: Atom
atom = Atom "C" (Point 1.0 2.0)

atom1 = Atom "C" (Point 1.0 2.0)
atom2 = Atom "O" (Point 3.0 4.0)
molecule = Molecule [atom1, atom2]

$(makeLenses ''Atom)
$(makeLenses ''Point)
$(makeLenses ''Molecule)

shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x+1) y)

shiftAtomX' :: Atom -> Atom
shiftAtomX' = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)
