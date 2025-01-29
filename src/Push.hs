module Push where

import Control.Lens
import Data.Map qualified as Map
-- import Instructions.IntInstructions
-- import Instructions.ExecInstructions
import State

-- import Debug.Trace (trace, traceStack)

-- Each core func should be: (State -> State -> State)
-- but each core function can use abstract helper functions.
-- That is more efficient than checking length.
-- Everntually, this can be part of the apply func to state helpers,
-- which should take the number and type of parameter they have.

-- This is one of the push genome functions itself, not infrastructure.
-- Optionally, split this off into independent functions
instructionParameterLoad :: State -> State
instructionParameterLoad state@(State {_parameter = (p : _)}) = case p of
  (GeneInt val) -> state & int .~ val : view int state
  (GeneFloat val) -> state & float .~ val : view float state
  (GeneBool val) -> state & bool .~ val : view bool state
  (GeneString val) -> state & string .~ val : view string state
  (GeneChar val) -> state & char .~ val : view char state
  (GeneVectorInt val) -> state & vectorInt .~ val : view vectorInt state
  (GeneVectorFloat val) -> state & vectorFloat .~ val : view vectorFloat state
  (GeneVectorBool val) -> state & vectorBool .~ val : view vectorBool state
  (GeneVectorString val) -> state & vectorString .~ val : view vectorString state
  (GeneVectorChar val) -> state & vectorChar .~ val : view vectorChar state
  (StateFunc _) -> undefined
  (PlaceInput _) -> undefined
  Close -> undefined
  (Block xs) -> state & exec .~ xs <> view exec state
instructionParameterLoad state = state

-- Loads a genome into the exec stack
loadProgram :: [Gene] -> State -> State
loadProgram newstack state = state & exec .~ newstack

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
interpretExec state@(State {_exec = e : es}) =
  case e of
    (GeneInt val) -> interpretExec (state & exec .~ es & int .~ val : view int state)
    (GeneFloat val) -> interpretExec (state & exec .~ es & float .~ val : view float state)
    (GeneBool val) -> interpretExec (state & exec .~ es & bool .~ val : view bool state)
    (GeneString val) -> interpretExec (state & exec .~ es & string .~ val : view string state)
    (GeneChar val) -> interpretExec (state & exec .~ es & char .~ val : view char state)
    (GeneVectorInt val) -> interpretExec (state & exec .~ es & vectorInt .~ val : view vectorInt state)
    (GeneVectorFloat val) -> interpretExec (state & exec .~ es & vectorFloat .~ val : view vectorFloat state)
    (GeneVectorBool val) -> interpretExec (state & exec .~ es & vectorBool .~ val : view vectorBool state)
    (GeneVectorString val) -> interpretExec (state & exec .~ es & vectorString .~ val : view vectorString state)
    (GeneVectorChar val) -> interpretExec (state & exec .~ es & vectorChar .~ val : view vectorChar state)
    (StateFunc func) -> interpretExec $ func state {_exec = es}
    (Block block) -> interpretExec (state {_exec = block ++ es})
    (PlaceInput val) -> interpretExec (state {_exec = (view input state Map.! val) : es})
    Close -> undefined -- remove Close constructor later?
interpretExec state = state

-- Need to make interpretExec strict, right?
