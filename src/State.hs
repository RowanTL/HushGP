{-# LANGUAGE TemplateHaskell #-}
module State where

import qualified Data.Map as Map
import Control.Lens

-- The exec stack must store heterogenous types,
-- and we must be able to detect that type at runtime.
-- One solution is for the exec stack to be a list of [Gene].
-- The parameter stack could be singular [Gene] or multiple [atomic] types.
data Gene
  = GeneInt Int
  | GeneFloat Float
  | GeneBool Bool
  | GeneString String
  | GeneIntVector [Int]
  | GeneFloatVector [Float]
  | GeneBoolVector [Bool]
  | GeneStringVector [String]
  | StateFunc (State -> State)
  | PlaceInput String
  | Close
  | Block [Gene]

instance Eq Gene where
  GeneInt x == GeneInt y = x == y
  GeneFloat x == GeneFloat y = x == y
  GeneBool x == GeneBool y = x == y
  GeneString x == GeneString y = x == y
  PlaceInput x == PlaceInput y = x == y
  GeneIntVector xs == GeneIntVector ys = xs == ys
  GeneFloatVector xs == GeneFloatVector ys = xs == ys 
  GeneBoolVector xs == GeneBoolVector ys = xs == ys
  GeneStringVector xs == GeneStringVector ys = xs == ys
  Close == Close = True
  StateFunc _ == StateFunc _ = True -- This line is probably not the best thing to do
  Block [x] == Block [y] = [x] == [y]
  _ == _ = False

instance Show Gene where
  show (GeneInt x) = "Int: " <> show x
  show (GeneFloat x) = "Float: " <> show x
  show (GeneBool x) = "Bool: " <> show x
  show (GeneString x) = "String: " <> x
  show (StateFunc _) = "Func: unnamed"
  show (PlaceInput x) = "In: " <> x
  show (GeneIntVector xs) = "Int Vec: " <> show xs
  show (GeneFloatVector xs) = "Float Vec: " <> show xs
  show (GeneBoolVector xs) = "Bool Vec: " <> show xs
  show (GeneStringVector xs) = "String Vec: " <> show xs
  show Close = "Close"
  show (Block xs) = "Block: " <> show xs

data State = State
  { _exec :: [Gene],
    _int :: [Int],
    _float :: [Float],
    _bool :: [Bool],
    _string :: [String],
    _intVector :: [[Int]],
    _floatVector :: [[Float]],
    _boolVector :: [[Bool]],
    _stringVector :: [[String]],
    _parameter :: [Gene],
    _input :: Map.Map String Gene
  }
  deriving (Show, Eq)

$(makeLenses ''State)

emptyState :: State
emptyState =
  State
    { _exec = [],
      _int = [],
      _float = [],
      _bool = [],
      _string = [],
      _parameter = [],
      _intVector = [],
      _floatVector = [],
      _boolVector = [],
      _stringVector = [],
      _input = Map.empty
    }

exampleState :: State
exampleState = 
  State
    { _exec = [],
      _int = [32, 56],
      _float = [3.23, 9.235],
      _bool = [True, False],
      _string = ["abc", "123"],
      _parameter = [],
      _intVector = [[1,2], [5,6,8]],
      _floatVector = [[1.234, 9.21], [5.42, 6.221, 8.5493]],
      _boolVector = [[True, False], [False, False, True]],
      _stringVector = [["def", "567"], ["gamer", "fellah", "live action how to train your dragon"]],
      _input = Map.empty
    }
