{-# LANGUAGE TemplateHaskell #-}

module LearnLens where

import Control.Lens hiding (element)
import Control.Lens.TH

data Atom = Atom {_element :: String, _point :: Point} deriving (Show)

data Point = Point {_x :: Double, _Y :: Double} deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)

myAtom :: Atom
myAtom = Atom "climberite" (Point 4.0 3.2)

shiftAtom :: Atom -> Atom
shiftAtom = over (point . x) (+ 1)

data Molecule = Molecule {_atoms :: [Atom]} deriving (Show)

$(makeLenses ''Molecule)

shiftMolecule :: Molecule -> Molecule
shiftMolecule = over (atoms . traverse . point . x) (+ 1)

-- Example without template haskell
defPoint :: Lens' Atom Point
defPoint = lens _point (\atom newPoint -> atom {_point = newPoint})
