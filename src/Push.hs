module Push where

import qualified Data.Map as Map

-- import Debug.Trace (trace, traceStack)

-- The exec stack must store heterogenous types,
-- and we must be able to detect that type at runtime.
-- One solution is for the exec stack to be a list of [Gene].
-- The parameter stack could be singular [Gene] or multiple [atomic] types.
data Gene
  = GeneInt Int
  | GeneFloat Float
  | GeneBool Bool
  | GeneString String
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
  Close == Close = True
  StateFunc x == StateFunc y = True -- This line is probably not the best thing to do
  Block [x] == Block [y] = [x] == [y]
  _ == _ = False

instance Show Gene where
  show (GeneInt x) = "Int: " <> show x
  show (GeneFloat x) = "Float: " <> show x
  show (GeneBool x) = "Bool: " <> show x
  show (GeneString x) = "String: " <> x
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
instructionIntAdd (State es (i1 : i2 : is) fs bs ss ps im) = State es (i2 + i1 : is) fs bs ss ps im
instructionIntAdd state = state

instructionIntSub :: State -> State
instructionIntSub (State es (i1 : i2 : is) fs bs ss ps im) = State es (i2 - i1 : is) fs bs ss ps im
instructionIntSub state = state

instructionIntMul :: State -> State
instructionIntMul (State es (i1 : i2 : is) fs bs ss ps im) = State es (i2 * i1 : is) fs bs ss ps im
instructionIntMul state = state

instructionIntDiv :: State -> State
instructionIntDiv (State es (i1 : i2 : is) fs bs ss ps im) = State es (i2 `div` i1 : is) fs bs ss ps im
instructionIntDiv state = state

instructionIntMod :: State -> State
instructionIntMod (State es (i1 : i2 : is) fs bs ss ps im) = State es (i2 `mod` i1 : is) fs bs ss ps im
instructionIntMod state = state

instructionIntMin :: State -> State
instructionIntMin state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = min i1 i2 : is}
instructionIntMin state = state

instructionIntMax :: State -> State
instructionIntMax state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = max i1 i2 : is}
instructionIntMax state = state

instructionIntInc :: State -> State
instructionIntInc state@(State es (i1 : is) fs bs ss ps im) = state {int = i1 + 1 : is}
instructionIntInc state = state

instructionIntDec :: State -> State
instructionIntDec state@(State es (i1 : is) fs bs ss ps im) = state {int = i1 - 1 : is}
instructionIntDec state = state

instructionIntLT :: State -> State
instructionIntLT state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = is, bool = (i1 < i2) : bs}
instructionIntLT state = state

instructionIntGT :: State -> State
instructionIntGT state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = is, bool = (i1 > i2) : bs}
instructionIntGT state = state

instructionIntLTE :: State -> State
instructionIntLTE state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = is, bool = (i1 <= i2) : bs}
instructionIntLTE state = state

instructionIntGTE :: State -> State
instructionIntGTE state@(State es (i1 : i2 : is) fs bs ss ps im) = state {int = is, bool = (i1 >= i2) : bs}
instructionIntGTE state = state

instructionIntPop :: State -> State
instructionIntPop state@(State es (i1 : is) fs bs ss ps im) = state {int = is}
instructionIntPop state = state

instructionExecIf :: State -> State
instructionExecIf (State (e1 : e2 : es) is fs (b : bs) ss ps im) =
  case b of
    True -> State (e1 : es) is fs bs ss ps im
    False -> State (e2 : es) is fs bs ss ps im
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup (State alles@(e0 : es) is fs bs ss pm im) =
  State (e0 : alles) is fs bs ss pm im
instructionExecDup state = state

instructionExecDoRange :: State -> State
instructionExecDoRange (State (e1 : es) (i0 : i1 : is) fs bs ss ps im) =
  if increment i0 i1 /= 0
    then State (e1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc instructionExecDoRange, e1] : es) (i1 : is) fs bs ss ps im
    else State (e1 : es) (i1 : is) fs bs ss ps im
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

instructionExecDoCount :: State -> State
instructionExecDoCount state@(State (e1 : es) (i1 : is) fs bs ss ps im) =
  if i1 < 1
    then state
    else state {exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc instructionExecDoRange, e1] : es, int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State (e1 : es) (i1 : is) fs bs ss ps im) =
  if i1 < 1
    then state
    else state {exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc instructionExecDoRange, Block [StateFunc instructionIntPop, e1]] : es, int = is}
instructionExecDoTimes state = state

instructionExecWhile :: State -> State
instructionExecWhile state@(State (e1 : es) is fs [] ss ps im) =
  state {exec = es}
instructionExecWhile state@(State alles@(e1 : es) is fs (b1 : bs) ss ps im) =
  if b1
    then state {exec = e1 : StateFunc instructionExecWhile : alles, bool = bs}
    else state {exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State alles@(e1 : es) is fs bs ss ps im) =
  state {exec = e1 : StateFunc instructionExecWhile : alles}
instructionExecDoWhile state = state

-- Eats the boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State (e1 : es) is fs (b1 : bs) ss ps im) =
  if not b1
    then state {exec = es, bool = bs}
    else state {bool = bs}
instructionExecWhen state = state

-- This is one of the push genome functions itself, not infrastructure.
-- Optionally, split this off into independent functions
instructionParameterLoad :: State -> State
instructionParameterLoad (State es is fs bs ss (p : ps) im) = case p of
  (GeneInt val) -> State es (val : is) fs bs ss ps im
  (GeneFloat val) -> State es is (val : fs) bs ss ps im
  (GeneBool val) -> State es is fs (val : bs) ss ps im
  (GeneString val) -> State es is fs bs (val : ss) ps im
instructionParameterLoad state = state

-- Loads a genome into the exec stack
loadProgram :: [Gene] -> State -> State
loadProgram newstack (State _ i f b s p im) = State newstack i f b s p im

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
interpretExec (State [] is fs bs ss ps im) = State [] is fs bs ss ps im
interpretExec (State (e : es) is fs bs ss ps im) =
  case e of
    (GeneInt val) -> interpretExec (State es (val : is) fs bs ss ps im)
    (GeneFloat val) -> interpretExec (State es is (val : fs) bs ss ps im)
    (GeneBool val) -> interpretExec (State es is fs (val : bs) ss ps im)
    (GeneString val) -> interpretExec (State es is fs bs (val : ss) ps im)
    (StateFunc func) -> interpretExec (func (State es is fs bs ss ps im))
    (Block block) -> interpretExec (State (block ++ es) is fs bs ss ps im)
    (PlaceInput input) -> interpretExec (State (im Map.! input : es) is fs bs ss ps im)

-- Need to make interpretExec strict, right?
