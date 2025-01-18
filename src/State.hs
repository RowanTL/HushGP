module State where

import qualified Data.Map as Map

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
  { exec :: [Gene],
    int :: [Int],
    float :: [Float],
    bool :: [Bool],
    string :: [String],
    vectorInt :: [[Int]],
    vectorFloat :: [[Float]],
    vectorBool :: [[Bool]],
    vectorString :: [[String]],
    parameter :: [Gene],
    input :: Map.Map String Gene
  }
  deriving (Show, Eq)

emptyState :: State
emptyState =
  State
    { exec = [],
      int = [],
      float = [],
      bool = [],
      string = [],
      parameter = [],
      vectorInt = [],
      vectorFloat = [],
      vectorBool = [],
      vectorString = [],
      input = Map.empty
    }

