module Instructions.LogicalInstructions where

import State

instructionBoolFromInt :: State -> State
instructionBoolFromInt state@(State {_int = (i : is), _bool = bs}) = state {_int = is, _bool = (i /= 0) : bs}
instructionBoolFromInt state = state

instructionBoolFromFloat :: State -> State
instructionBoolFromFloat state@(State {_float = (f : fs), _bool = bs}) = state {_float = fs, _bool = (f /= 0) : bs}
instructionBoolFromFloat state = state

boolTemplate :: (Bool -> Bool -> Bool) -> State -> State
boolTemplate func state@(State {_bool = (b1 : b2 : bs)}) = state {_bool = func b1 b2 : bs}
boolTemplate _ state = state

instructionBoolAnd :: State -> State
instructionBoolAnd = boolTemplate (&&)

instructionBoolOr :: State -> State
instructionBoolOr = boolTemplate (||)

-- no builtin haskell xor moment
xor :: Bool -> Bool -> Bool
xor b1 b2
  | b1 && not b2 = True
  | not b1 && b2 = True
  | otherwise = False

instructionBoolXor :: State -> State
instructionBoolXor = boolTemplate xor
