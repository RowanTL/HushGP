module Push where

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
stackUpdate _ state = state

-- stackUpdate stackFunc (Input? _ : xs) (State e i f b s _)  = State e i f b s newstack

unpackIntGene :: Gene -> Int
unpackIntGene (IntGene item) = item

-- Start with monolithic add function this:
intAdd :: State -> State
intAdd state =
  let result = sum (map unpackIntGene (take 2 (int state)))
      dropped = drop 2 (int state)
   in stackUpdate (IntGene result : dropped) state

-- Later, generalize an applyFuncToState which applies simplifications of each simpler, modular atomic function:

-- intAdd :: (Int, Int) -> Int

-- applyFuncState :: State -> AtomicFuncTypes -> State

interpretGenome :: State -> [(State -> State)] -> State
interpretGenome state = foldl (\acc f -> f acc) state
