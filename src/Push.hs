module Push where

-- import Debug.Trace (trace, traceStack)

-- The exec stack must store heterogenous types,
-- and we must be able to detect that type at runtime.
-- One solution is for the exec stack to be a list of [Gene].
-- The parameter stack could be singular [Gene] or multiple [atomic] types.
data Gene
  = IntGene Int
  | FloatGene Float
  | BoolGene Bool
  | StringGene String
  | StateFunc (State -> State)
  | Close

--  | Block [Gene]
-- If we do plushy (as opposed to just detecting the Close itself,
-- then we may need to make a structually recursive data structure for the "program" data structure
-- exampleGenome = [Program] rather than [Gene], or just include the Block above?

data State = State
  { exec :: [Gene],
    int :: [Int],
    float :: [Float],
    bool :: [Bool],
    string :: [String],
    parameter :: [Gene]
  }

emptyState :: State
emptyState =
  State
    { exec = [],
      int = [],
      float = [],
      bool = [],
      string = [],
      parameter = []
    }

-- Each core func should be: (State -> State -> State)
-- but each core function can use abstract helper functions.
-- That is more efficient than checking length.
-- Everntually, this can be part of the apply func to state helpers,
-- which should take the number and type of parameter they have.
intAdd :: State -> State
intAdd (State es [] fs bs ss ps) = State es [] fs bs ss ps
intAdd (State es [i] fs bs ss ps) = State es [i] fs bs ss ps
intAdd (State es (i : is) fs bs ss ps) = State es ((i + head is) : drop 1 is) fs bs ss ps

--  let result = sum (take 2 (int state))
--      dropped = drop 2 (int state)
--   in updateIntStack (result : dropped) state

-- For safety, pattern match on [] and i:is or check for <2 long list after take 2?

-- Optionally, split this off into independent functions
parameterLoad :: State -> State
parameterLoad (State es is fs bs ss []) = State es is fs bs ss []
parameterLoad (State es is fs bs ss (p : ps)) = case p of
  (IntGene val) -> State es (val : is) fs bs ss ps
  (FloatGene val) -> State es is (val : fs) bs ss ps
  (BoolGene val) -> State es is fs (val : bs) ss ps
  (StringGene val) -> State es is fs bs (val : ss) ps

-- Wow, a one-liner for interpreting a paretheses-free genome...
-- Does not handle any data elements in genome yet,
-- but condition could be added to the lambda.
-- Need to update this when adding parethetical blocks too.
-- interpretFuncOnlyGenome :: State -> [State -> State] -> State
-- interpretFuncOnlyGenome = foldl' (\acc f -> f acc)
-- While this is not usable, it illustrates we want this pattern:
-- foldl (strict, cumulative accumulator), and not this pattern:
-- foldr (greedy/lazy incremental or quit early)

-- Loads a genome into the exec stack
loadProgarm :: [Gene] -> State -> State
loadProgarm newstack (State _ i f b s p) = State newstack i f b s p

-- Takes a Push state, and generates the next push state via:
-- If the first item on the EXEC stack is a single instruction
--     then pop it and execute it.
-- Else if the first item on the EXEC stack is a literal
--     then pop it and push it onto the appropriate stack.
-- Else (the first item must be a list) pop it and push all of the
--     items that it contains back onto the EXEC stack individually,
--     in reverse order (so that the item that was first in the list
--     ends up on top).
interpretExec :: State -> State
interpretExec (State [] is fs bs ss ps) = State [] is fs bs ss ps
interpretExec (State (e : es) is fs bs ss ps) =
  let poppedState = State es is fs bs ss ps
   in case e of
        (IntGene val) -> interpretExec (State es (val : is) fs bs ss ps)
        (FloatGene val) -> interpretExec (State es is (val : fs) bs ss ps)
        (BoolGene val) -> interpretExec (State es is fs (val : bs) ss ps)
        (StringGene val) -> interpretExec (State es is fs bs (val : ss) ps)
        (StateFunc func) -> interpretExec (func poppedState)

-- The safety of interpretExec on empty stacks depends on the functions it calls.
-- Need to make interpretExec strict, right?
