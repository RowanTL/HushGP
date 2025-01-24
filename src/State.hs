{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens
import Data.Map qualified as Map

-- The exec stack must store heterogenous types,
-- and we must be able to detect that type at runtime.
-- One solution is for the exec stack to be a list of [Gene].
-- The parameter stack could be singular [Gene] or multiple [atomic] types.
data Gene
  = GeneInt Int
  | GeneFloat Float
  | GeneBool Bool
  | GeneString String
  | GeneChar Char
  | GeneIntVector [Int]
  | GeneFloatVector [Float]
  | GeneBoolVector [Bool]
  | GeneStringVector [String]
  | GeneCharVector [Char]
  | StateFunc (State -> State)
  | PlaceInput String
  | Close
  | Block [Gene]

instance Eq Gene where
  GeneInt x == GeneInt y = x == y
  GeneFloat x == GeneFloat y = x == y
  GeneBool x == GeneBool y = x == y
  GeneString x == GeneString y = x == y
  GeneChar x == GeneChar y = x == y
  PlaceInput x == PlaceInput y = x == y
  GeneIntVector xs == GeneIntVector ys = xs == ys
  GeneFloatVector xs == GeneFloatVector ys = xs == ys
  GeneBoolVector xs == GeneBoolVector ys = xs == ys
  GeneStringVector xs == GeneStringVector ys = xs == ys
  GeneCharVector xs == GeneCharVector ys = xs == ys
  Close == Close = True
  StateFunc _ == StateFunc _ = True -- This line is probably not the best thing to do
  Block x == Block y = x == y
  _ == _ = False

instance Show Gene where
  show (GeneInt x) = "Int: " <> show x
  show (GeneFloat x) = "Float: " <> show x
  show (GeneBool x) = "Bool: " <> show x
  show (GeneString x) = "String: " <> x
  show (GeneChar x) = "Char: " <> show x
  show (StateFunc _) = "Func: unnamed"
  show (PlaceInput x) = "In: " <> x
  show (GeneIntVector xs) = "Int Vec: " <> show xs
  show (GeneFloatVector xs) = "Float Vec: " <> show xs
  show (GeneBoolVector xs) = "Bool Vec: " <> show xs
  show (GeneStringVector xs) = "String Vec: " <> show xs
  show (GeneCharVector xs) = "Char Vec: " <> show xs
  show Close = "Close"
  show (Block xs) = "Block: " <> show xs

data State = State
  { _exec :: [Gene],
    _code :: [Gene],
    _int :: [Int],
    _float :: [Float],
    _bool :: [Bool],
    _string :: [String],
    _char :: [Char],
    _intVector :: [[Int]],
    _floatVector :: [[Float]],
    _boolVector :: [[Bool]],
    _stringVector :: [[String]],
    _charVector :: [[Char]],
    _parameter :: [Gene],
    _input :: Map.Map String Gene
  }
  deriving (Show, Eq)

$(makeLenses ''State)

emptyState :: State
emptyState =
  State
    { _exec = [],
      _code = [],
      _int = [],
      _float = [],
      _bool = [],
      _string = [],
      _char = [],
      _parameter = [],
      _intVector = [],
      _floatVector = [],
      _boolVector = [],
      _stringVector = [],
      _charVector = [],
      _input = Map.empty
    }

exampleState :: State
exampleState =
  State
    { _exec = [],
      _code = [],
      _int = [32, 56],
      _float = [3.23, 9.235],
      _bool = [True, False],
      _string = ["abc", "123"],
      _char = ['d', 'e', 'f'],
      _parameter = [],
      _intVector = [[1, 2], [5, 6, 8]],
      _floatVector = [[1.234, 9.21], [5.42, 6.221, 8.5493]],
      _boolVector = [[True, False], [False, False, True]],
      _stringVector = [["this is a sentence", "this is also a sentence"], ["s0", "s1", "s2"]],
      _charVector = [['z', 'x'], ['r', 'a', 't', 'l']],
      _input = Map.empty
    }
