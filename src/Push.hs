{-# LANGUAGE DataKinds #-}
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
  deriving Show

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
  then State (e1 : Block [IntGene (i1 + increment i0 i1), IntGene i0, StateFunc instructionExecDoRange, e1] : es) (i1 : is) fs bs ss ps im
  else State (e1 : es) (i1 : is) fs bs ss ps im
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

-- This is one of the push genome functions itself, not infrastructure.
-- Optionally, split this off into independent functions
instructionParameterLoad :: State -> State
instructionParameterLoad (State es is fs bs ss (p : ps) im) = case p of
  (IntGene val) -> State es (val : is) fs bs ss ps im
  (FloatGene val) -> State es is (val : fs) bs ss ps im
  (BoolGene val) -> State es is fs (val : bs) ss ps im
  (StringGene val) -> State es is fs bs (val : ss) ps im
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
    (IntGene val) -> interpretExec (State es (val : is) fs bs ss ps im)
    (FloatGene val) -> interpretExec (State es is (val : fs) bs ss ps im)
    (BoolGene val) -> interpretExec (State es is fs (val : bs) ss ps im)
    (StringGene val) -> interpretExec (State es is fs bs (val : ss) ps im)
    (StateFunc func) -> interpretExec (func (State es is fs bs ss ps im))
    (Block block) -> interpretExec (State (block ++ es) is fs bs ss ps im)
    (PlaceInput input) -> interpretExec (State (im Map.! input : es) is fs bs ss ps im)

-- Need to make interpretExec strict, right?
