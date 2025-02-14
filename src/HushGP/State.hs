{-# LANGUAGE TemplateHaskell #-}

module HushGP.State where

import Control.Lens hiding (elements)
import Data.Map qualified as Map
import System.Random

-- |The exec stack must store heterogenous types,
--  and we must be able to detect that type at runtime.
--  One solution is for the exec stack to be a list of [Gene].
--  The parameter stack could be singular [Gene] or multiple [atomic] types.
data Gene
  = GeneInt Integer
  | GeneFloat Double
  | GeneBool Bool
  | GeneString String
  | GeneChar Char
  | GeneVectorInt [Integer]
  | GeneVectorFloat [Double]
  | GeneVectorBool [Bool]
  | GeneVectorString [String]
  | GeneVectorChar [Char]
  -- |State -> State is the function itself. String stores the name of the function.
  | StateFunc (State -> State, String)
  | PlaceInput String
  | Close
  | Open Int
  | Block [Gene]
  | GeneIntERC (Integer, StdGen)
  | GeneFloatERC (Double, StdGen)
  | GeneBoolERC (Bool, StdGen)
  | GeneStringERC (String, StdGen)
  | GeneCharERC (Char, StdGen)
  | GeneVectorIntERC ([Integer], StdGen)
  | GeneVectorFloatERC ([Double], StdGen)
  | GeneVectorBoolERC ([Bool], StdGen)
  | GeneVectorStringERC ([String], StdGen)
  | GeneVectorCharERC ([Char], StdGen)

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
  Open x == Open y = x == y
  StateFunc (_, nameX) == StateFunc (_, nameY) = nameX == nameY
  Block x == Block y = x == y
  GeneIntERC (x, _) == GeneIntERC (y, _) = x == y
  GeneFloatERC (x, _) == GeneFloatERC (y, _) = x == y
  GeneBoolERC (x, _) == GeneBoolERC (y, _) = x == y
  GeneStringERC (x, _) == GeneStringERC (y, _) = x == y
  GeneCharERC (x, _) == GeneCharERC (y, _) = x == y
  GeneVectorIntERC (x, _) == GeneVectorIntERC (y, _) = x == y
  GeneVectorFloatERC (x, _) == GeneVectorFloatERC (y, _) = x == y
  GeneVectorBoolERC (x, _) == GeneVectorBoolERC (y, _) = x == y
  GeneVectorStringERC (x, _) == GeneVectorStringERC (y, _) = x == y
  GeneVectorCharERC (x, _) == GeneVectorCharERC (y, _) = x == y
  GeneIntERC (x, _) == GeneInt y = x == y
  GeneFloatERC (x, _) == GeneFloat y = x == y
  GeneBoolERC (x, _) == GeneBool y = x == y
  GeneStringERC (x, _) == GeneString y = x == y
  GeneCharERC (x, _) == GeneChar y = x == y
  GeneVectorIntERC (x, _) == GeneVectorInt y = x == y
  GeneVectorFloatERC (x, _) == GeneVectorFloat y = x == y
  GeneVectorBoolERC (x, _) == GeneVectorBool y = x == y
  GeneVectorStringERC (x, _) == GeneVectorString y = x == y
  GeneVectorCharERC (x, _) == GeneVectorChar y = x == y
  _ == _ = False

instance Ord Gene where
  GeneInt x <= GeneInt y = x <= y
  GeneFloat x <= GeneFloat y = x <= y
  GeneBool x <= GeneBool y = x <= y
  GeneString x <= GeneString y = x <= y
  GeneChar x <= GeneChar y = x <= y
  PlaceInput x <= PlaceInput y = x <= y
  GeneVectorInt xs <= GeneVectorInt ys = xs <= ys
  GeneVectorFloat xs <= GeneVectorFloat ys = xs <= ys
  GeneVectorBool xs <= GeneVectorBool ys = xs <= ys
  GeneVectorString xs <= GeneVectorString ys = xs <= ys
  GeneVectorChar xs <= GeneVectorChar ys = xs <= ys
  Close <= Close = True
  Open x <= Open y = x <= y
  StateFunc (_, nameX) <= StateFunc (_, nameY) = nameX <= nameY
  Block x <= Block y = x <= y
  GeneIntERC (x, _) <= GeneIntERC (y, _) = x <= y
  GeneFloatERC (x, _) <= GeneFloatERC (y, _) = x <= y
  GeneBoolERC (x, _) <= GeneBoolERC (y, _) = x <= y
  GeneStringERC (x, _) <= GeneStringERC (y, _) = x <= y
  GeneCharERC (x, _) <= GeneCharERC (y, _) = x <= y
  GeneVectorIntERC (x, _) <= GeneVectorIntERC (y, _) = x <= y
  GeneVectorFloatERC (x, _) <= GeneVectorFloatERC (y, _) = x <= y
  GeneVectorBoolERC (x, _) <= GeneVectorBoolERC (y, _) = x <= y
  GeneVectorStringERC (x, _) <= GeneVectorStringERC (y, _) = x <= y
  GeneVectorCharERC (x, _) <= GeneVectorCharERC (y, _) = x <= y
  GeneIntERC (x, _) <= GeneInt y = x <= y
  GeneFloatERC (x, _) <= GeneFloat y = x <= y
  GeneBoolERC (x, _) <= GeneBool y = x <= y
  GeneStringERC (x, _) <= GeneString y = x <= y
  GeneCharERC (x, _) <= GeneChar y = x <= y
  GeneVectorIntERC (x, _) <= GeneVectorInt y = x <= y
  GeneVectorFloatERC (x, _) <= GeneVectorFloat y = x <= y
  GeneVectorBoolERC (x, _) <= GeneVectorBool y = x <= y
  GeneVectorStringERC (x, _) <= GeneVectorString y = x <= y
  GeneVectorCharERC (x, _) <= GeneVectorChar y = x <= y
  _ <= _ = False

instance Show Gene where
  show (GeneInt x) = "Int: " <> show x
  show (GeneFloat x) = "Float: " <> show x
  show (GeneBool x) = "Bool: " <> show x
  show (GeneString x) = "String: " <> x
  show (GeneChar x) = "Char: " <> show x
  show (StateFunc (_, funcName)) = "Func: " <> funcName
  show (PlaceInput x) = "In: " <> show x
  show (GeneVectorInt xs) = "Int Vec: " <> show xs
  show (GeneVectorFloat xs) = "Float Vec: " <> show xs
  show (GeneVectorBool xs) = "Bool Vec: " <> show xs
  show (GeneVectorString xs) = "String Vec: " <> show xs
  show (GeneVectorChar xs) = "Char Vec: " <> show xs
  show Close = "Close"
  show (Open x) = "Open: " <> show x
  show (Block xs) = "Block: " <> show xs
  show (GeneIntERC x) = "Int ERC: " <> show x
  show (GeneFloatERC x) = "Float ERC: " <> show x
  show (GeneBoolERC x) = "Bool ERC: " <> show x
  show (GeneStringERC x) = "String ERC: " <> show x
  show (GeneCharERC x) = "Char ERC: " <> show x
  show (GeneVectorIntERC x) = "Int Vec ERC: " <> show x
  show (GeneVectorFloatERC x) = "Float Vec ERC: " <> show x
  show (GeneVectorBoolERC x) = "Bool Vec ERC: " <> show x
  show (GeneVectorStringERC x) = "String Vec ERC: " <> show x
  show (GeneVectorCharERC x) = "Char Vec ERC: " <> show x

-- | The structure that holds all of the values.
data State = State
  { _exec :: [Gene],
    _code :: [Gene],
    _int :: [Integer],
    _float :: [Double],
    _bool :: [Bool],
    _string :: [String],
    _char :: [Char],
    _vectorInt :: [[Integer]],
    _vectorFloat :: [[Double]],
    _vectorBool :: [[Bool]],
    _vectorString :: [[String]],
    _vectorChar :: [[Char]],
    _parameter :: [Gene],
    _input :: Map.Map String Gene
  }
  deriving (Show, Eq, Ord)

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
      _int = [32, 56, 88, 91],
      _float = [3.23, 9.235, 5.3211, 8.0],
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

-- This must stay at the end of the file.
-- Template haskell seems to be messing with GHC.Generics
$(makeLenses ''State)
