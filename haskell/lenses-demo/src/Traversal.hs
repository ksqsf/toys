{-# LANGUAGE TemplateHaskell #-}

module Traversal where

import Control.Lens hiding (element)
import Control.Lens.TH

data Atom = Atom { _element :: String, _point :: Point } deriving (Show, Eq)
data Point = Point { _x :: Double, _y :: Double } deriving (Show, Eq)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)
$(makeLenses ''Molecule)

moleculeX :: Traversal' Molecule Double
moleculeX = atoms . traverse . point . x

atom1 = Atom "C" (Point 1.0 2.0)
atom2 = Atom "O" (Point 3.0 4.0)
molecule = Molecule [atom1, atom2]

