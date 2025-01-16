{-# LANGUAGE RecordWildCards #-}

module Push where

-- import Debug.Trace (trace, traceStack)
import qualified Data.Map as Map

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
  | PlaceInput String
  | Close
  | Block [Gene]

instance Eq Gene where
  IntGene x == IntGene y = x == y
  FloatGene x == FloatGene y = x == y
  BoolGene x == BoolGene y = x == y
  StringGene x == StringGene y = x == y
  PlaceInput x == PlaceInput y = x == y
  Close == Close = True
  StateFunc x == StateFunc y = True -- This line is probably not the best thing to do
  Block [x] == Block [y] = [x] == [y]
  _ == _ = False

instance Show Gene where
  show (IntGene x) = "Int: " <> show x
  show (FloatGene x) = "Float: " <> show x
  show (BoolGene x) = "Bool: " <> show x
  show (StringGene x) = "String: " <> x
  show (StateFunc func) = "Func: unnamed"
  show (PlaceInput x) = "In: " <> x
  show Close = "Close"
  show (Block xs) = "Block: " <> show xs

data State = State
  { exec :: [Gene],
    int :: [Int],
    float :: [Float],
    bool :: [Bool],
    string :: [String],
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
      input = Map.empty
    }

-- Each core func should be: (State -> State -> State)
-- but each core function can use abstract helper functions.
-- That is more efficient than checking length.
-- Everntually, this can be part of the apply func to state helpers,
-- which should take the number and type of parameter they have.
instructionIntAdd :: State -> State
instructionIntAdd state@(State {int = (i1 : i2 : is), ..}) = state {int = i1 + i2 : is}
instructionIntAdd state = state

instructionIntSub :: State -> State
instructionIntSub state@(State {int = (i1 : i2 : is), ..}) = state {int = i1 - i2 : is}
instructionIntSub state = state

instructionIntMul :: State -> State
instructionIntMul state@(State {int = (i1 : i2 : is), ..}) = state {int = i1 * i2 : is}
instructionIntMul state = state

instructionIntDiv :: State -> State
instructionIntDiv state@(State {int = (i1 : i2 : is), ..}) = state {int = i1 `div` i2 : is}
instructionIntDiv state = state

instructionIntMod :: State -> State
instructionIntMod state@(State {int = (i1 : i2 : is), ..}) = state {int = i1 `mod` i2 : is}
instructionIntMod state = state

instructionIntMin :: State -> State
instructionIntMin state@(State {int = (i1 : i2 : is), ..}) = state {int = min i1 i2 : is}
instructionIntMin state = state

instructionIntMax :: State -> State
instructionIntMax state@(State {int = (i1 : i2 : is), ..}) = state {int = max i1 i2 : is}
instructionIntMax state = state

instructionIntInc :: State -> State
instructionIntInc state@(State {int = (i1 : is), ..}) = state {int = i1 + 1 : is}
instructionIntInc state = state

instructionIntDec :: State -> State
instructionIntDec state@(State {int = (i1 : is), ..}) = state {int = i1 - 1 : is}
instructionIntDec state = state

instructionIntLT :: State -> State
instructionIntLT state@(State {int = i1 : i2 : is, bool = bs, ..}) = state {int = is, bool = (i1 < i2) : bs}
instructionIntLT state = state

instructionIntGT :: State -> State
instructionIntGT state@(State {int = i1 : i2 : is, bool = bs, ..}) = state {int = is, bool = (i1 > i2) : bs}
instructionIntGT state = state

instructionIntLTE :: State -> State
instructionIntLTE state@(State {int = i1 : i2 : is, bool = bs, ..}) = state {int = is, bool = (i1 <= i2) : bs}
instructionIntLTE state = state

instructionIntGTE :: State -> State
instructionIntGTE state@(State {int = i1 : i2 : is, bool = bs, ..}) = state {int = is, bool = (i1 >= i2) : bs}
instructionIntGTE state = state

instructionIntPop :: State -> State
instructionIntPop state@(State {int = (i1 : is), ..}) = state {int = is}
instructionIntPop state = state

instructionExecIf :: State -> State
instructionExecIf state@(State {exec = (e1 : e2 : es), bool = (b : bs), ..}) =
  if b
    then state {exec = e1 : es}
    else state {exec = e2 : es}
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup state@(State {exec = alles@(e0 : es), ..}) =
  state {exec = e0 : alles}
instructionExecDup state = state

instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {exec = (e1 : es), int = (i0 : i1 : is), ..}) =
  if increment i0 i1 /= 0
    then state {exec = e1 : Block [IntGene (i1 + increment i0 i1), IntGene i0, StateFunc instructionExecDoRange, e1] : es, int = i1 : is}
    else state {exec = e1 : es, int = i1 : is}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

instructionExecDoCount :: State -> State
instructionExecDoCount state@(State {exec = (e1 : es), int = (i1 : is), ..}) =
  if i1 < 1
    then state
    else state {exec = Block [IntGene 0, IntGene $ i1 - 1, StateFunc instructionExecDoRange, e1] : es, int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {exec = (e1 : es), int = (i1 : is), ..}) =
  if i1 < 1
    then state
    else state {exec = Block [IntGene 0, IntGene $ i1 - 1, StateFunc instructionExecDoRange, Block [StateFunc instructionIntPop, e1]] : es, int = is}
instructionExecDoTimes state = state

instructionExecWhile :: State -> State
instructionExecWhile state@(State {exec = (e1 : es), bool = [], ..}) =
  state {exec = es}
instructionExecWhile state@(State {exec = alles@(e1 : es), bool = (b1 : bs), ..}) =
  if b1
    then state {exec = e1 : StateFunc instructionExecWhile : alles, bool = bs}
    else state {exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {exec = alles@(e1 : es), ..}) =
  state {exec = e1 : StateFunc instructionExecWhile : alles}
instructionExecDoWhile state = state

-- Eats the boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State {exec = (e1 : es), bool = (b1 : bs), ..}) =
  if not b1
    then state {exec = es, bool = bs}
    else state {bool = bs}
instructionExecWhen state = state

-- This is one of the push genome functions itself, not infrastructure.
-- Optionally, split this off into independent functions
instructionParameterLoad :: State -> State
instructionParameterLoad state@(State {parameter = (p : ps), ..}) = case p of
  (IntGene val) -> state {int = val : int}
  (FloatGene val) -> state {float = val : float}
  (BoolGene val) -> state {bool = val : bool}
  (StringGene val) -> state {string = val : string}
instructionParameterLoad state = state

-- Loads a genome into the exec stack
loadProgram :: [Gene] -> State -> State
loadProgram newstack state@(State {exec = _, ..}) = state {exec = newstack}

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
interpretExec state@(State {exec = [], ..}) = state {exec = []}
interpretExec state@(State {exec = (e : es), ..}) =
  case e of
    (IntGene val) -> interpretExec state {int = val : int}
    (FloatGene val) -> interpretExec (state {float = val : float})
    (BoolGene val) -> interpretExec (state {bool = val : bool})
    (StringGene val) -> interpretExec (state {string = val : string})
    (StateFunc func) -> interpretExec $ func state
    (Block block) -> interpretExec (state {exec = block ++ es})
    (PlaceInput val) -> interpretExec (state {exec = (input Map.! val) : es})

-- Need to make interpretExec strict, right?
