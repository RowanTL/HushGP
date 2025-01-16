{-# LANGUAGE RecordWildCards #-}

module Push where

import qualified Data.Map as Map
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
instructionParameterLoad state@(State {parameter = (p : _), ..}) = case p of
  (GeneInt val) -> state {int = val : int}
  (GeneFloat val) -> state {float = val : float}
  (GeneBool val) -> state {bool = val : bool}
  (GeneString val) -> state {string = val : string}
  (StateFunc _) -> undefined
  (PlaceInput _) -> undefined
  Close -> undefined
  (Block xs) -> state {exec = xs <> exec}
instructionParameterLoad state = state

-- Loads a genome into the exec stack
loadProgram :: [Gene] -> State -> State
loadProgram newstack state@(State {exec = _}) = state {exec = newstack}

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
interpretExec state@(State {exec = []}) = state {exec = []}
interpretExec state@(State {exec = (e : es), ..}) =
  case e of
    (GeneInt val) -> interpretExec state {exec = es, int = val : int}
    (GeneFloat val) -> interpretExec (state {exec = es, float = val : float})
    (GeneBool val) -> interpretExec (state {exec = es, bool = val : bool})
    (GeneString val) -> interpretExec (state {exec = es, string = val : string})
    (StateFunc func) -> interpretExec $ func state {exec = es}
    (Block block) -> interpretExec (state {exec = block ++ es})
    (PlaceInput val) -> interpretExec (state {exec = (input Map.! val) : es})
    Close -> undefined -- remove Close constructor later?

-- Need to make interpretExec strict, right?
