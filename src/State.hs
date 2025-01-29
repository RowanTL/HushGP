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
  | GeneVectorInt [Int]
  | GeneVectorFloat [Float]
  | GeneVectorBool [Bool]
  | GeneVectorString [String]
  | GeneVectorChar [Char]
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
  GeneVectorInt xs == GeneVectorInt ys = xs == ys
  GeneVectorFloat xs == GeneVectorFloat ys = xs == ys
  GeneVectorBool xs == GeneVectorBool ys = xs == ys
  GeneVectorString xs == GeneVectorString ys = xs == ys
  GeneVectorChar xs == GeneVectorChar ys = xs == ys
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
  show (GeneVectorInt xs) = "Int Vec: " <> show xs
  show (GeneVectorFloat xs) = "Float Vec: " <> show xs
  show (GeneVectorBool xs) = "Bool Vec: " <> show xs
  show (GeneVectorString xs) = "String Vec: " <> show xs
  show (GeneVectorChar xs) = "Char Vec: " <> show xs
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
    _vectorInt :: [[Int]],
    _vectorFloat :: [[Float]],
    _vectorBool :: [[Bool]],
    _vectorString :: [[String]],
    _vectorChar :: [[Char]],
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
      _vectorInt = [],
      _vectorFloat = [],
      _vectorBool = [],
      _vectorString = [],
      _vectorChar = [],
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
      _vectorInt = [[1, 2], [5, 6, 8]],
      _vectorFloat = [[1.234, 9.21], [5.42, 6.221, 8.5493]],
      _vectorBool = [[True, False], [False, False, True]],
      _vectorString = [["this is a sentence", "this is also a sentence"], ["s0", "s1", "s2"]],
      _vectorChar = [['z', 'x'], ['r', 'a', 't', 'l']],
      _input = Map.empty
    }
