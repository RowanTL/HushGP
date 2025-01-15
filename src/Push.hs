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
  | Block [Gene]

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
instructionIntAdd :: State -> State
instructionIntAdd (State es (i1 : i2 : is) fs bs ss ps) = State es (i2 + i1 : is) fs bs ss ps
instructionIntAdd state = state

instructionIntSub :: State -> State
instructionIntSub (State es (i1 : i2 : is) fs bs ss ps) = State es (i2 - i1 : is) fs bs ss ps
instructionIntSub state = state

instructionIntMul :: State -> State
instructionIntMul (State es (i1 : i2 : is) fs bs ss ps) = State es (i2 * i1 : is) fs bs ss ps
instructionIntMul state = state

instructionIntDiv :: State -> State
instructionIntDiv (State es (i1 : i2 : is) fs bs ss ps) = State es (i2 `div` i1 : is) fs bs ss ps
instructionIntDiv state = state

instructionExecIf :: State -> State
instructionExecIf (State (e : es) is fs (b : bs) ss ps) =
  case b of
    True -> State (e : drop 1 es) is fs bs ss ps
    False -> State (es) is fs bs ss ps
instructionExecIf state = state

-- This is one of the push genome functions itself, not infrastructure.
-- Optionally, split this off into independent functions
instructionParameterLoad :: State -> State
instructionParameterLoad (State es is fs bs ss (p : ps)) = case p of
  (IntGene val) -> State es (val : is) fs bs ss ps
  (FloatGene val) -> State es is (val : fs) bs ss ps
  (BoolGene val) -> State es is fs (val : bs) ss ps
  (StringGene val) -> State es is fs bs (val : ss) ps
instructionParameterLoad state = state

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
-- The empty-stack safety of interpretExec on empty stacks depends on the functions it calls.
interpretExec :: State -> State
interpretExec (State [] is fs bs ss ps) = State [] is fs bs ss ps
interpretExec (State (e : es) is fs bs ss ps) =
  case e of
    (IntGene val) -> interpretExec (State es (val : is) fs bs ss ps)
    (FloatGene val) -> interpretExec (State es is (val : fs) bs ss ps)
    (BoolGene val) -> interpretExec (State es is fs (val : bs) ss ps)
    (StringGene val) -> interpretExec (State es is fs bs (val : ss) ps)
    (StateFunc func) -> interpretExec (func (State es is fs bs ss ps))
    (Block block) -> interpretExec (State (block ++ es) is fs bs ss ps)

-- Need to make interpretExec strict, right?
