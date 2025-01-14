module Push where

import Data.List (foldl')

-- import Debug.Trace (trace, traceStack)

-- GeneModular or Gene?
-- Should we use a StateFunc or *Func for each push type?
-- Start with whole StateFunc since it is monolithic (easier to start),
-- then generalize and abstract with an apply method that itself takes a simpler function and the state?
{-
data GeneModular
  = IntGene Int
  | FloatGene Float
  | BoolGene Bool
  | StringGene String
  | IntFunc [([Int] -> [Int] -> [Int])]
  | StrFunc [([String] -> [String] -> [String])]
  | BoolFunc [([Bool] -> [Bool] -> [Bool])]
  | FloatFunc [([Float] -> [Float] -> [Float])]
-}

data Gene
  = IntGene Int
  | FloatGene Float
  | BoolGene Bool
  | StringGene String
  | StateFunc (State -> State -> State)
  | Close
  | Input Gene

--  | Group [Gene]
-- If we do plushy,
-- then we may need to make a structually recursive data structure for the "program" data structure
-- exampleGenome = [Program] rather than [Gene], or just include the Group above?

data State = State
  { exec :: [Gene],
    int :: [Gene],
    float :: [Gene],
    bool :: [Gene],
    string :: [Gene],
    input :: [Gene]
  }

emptyState :: State
emptyState =
  State
    { exec = [],
      int = [],
      float = [],
      bool = [],
      string = [],
      input = []
    }

stackUpdate :: [Gene] -> State -> State
stackUpdate newstack@(StateFunc _ : _) (State _ i f b s p) = State newstack i f b s p
stackUpdate newstack@(IntGene _ : _) (State e _ f b s p) = State e newstack f b s p
stackUpdate newstack@(FloatGene _ : _) (State e i _ b s p) = State e i newstack b s p
stackUpdate newstack@(BoolGene _ : _) (State e i f _ s p) = State e i f newstack s p
stackUpdate newstack@(StringGene _ : _) (State e i f b _ p) = State e i f b newstack p
stackUpdate newstack@(Input _ : _) (State e i f b s _) = State e i f b s newstack
stackUpdate _ state = state

unpackIntGene :: Gene -> Int
unpackIntGene (IntGene item) = item

-- Start with monolithic intAdd function:
intAdd :: State -> State
intAdd state =
  let result = sum (map unpackIntGene (take 2 (int state)))
      dropped = drop 2 (int state)
   in stackUpdate (IntGene result : dropped) state

-- Later, generalize a function called applyFuncToState,
-- which takes each simpler atomic function, and the state,
-- and applies the function to the state, for example:
-- intAdd :: (Int, Int) -> Int
-- applyFuncState :: AtomicFuncTypes -> State -> State
-- this would change Gene to something like GeneModular above.

-- Wow, a one-liner for interpreting a paretheses-free genome...
-- Does not handle any data elements in genome yet,
-- but condition could be added to the lambda.
-- Need to update this when adding parethetical blocks too.
interpretGenome :: State -> [(State -> State)] -> State
interpretGenome state = foldl' (\acc f -> f acc) state
