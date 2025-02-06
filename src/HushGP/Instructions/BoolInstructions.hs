module HushGP.Instructions.BoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

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

instructionBoolInvertFirstThenAnd :: State -> State
instructionBoolInvertFirstThenAnd state@(State {_bool = (b1 : bs)}) = boolTemplate (&&) state {_bool = not b1 : bs}
instructionBoolInvertFirstThenAnd state = state

instructionBoolInvertSecondThenAnd :: State -> State
instructionBoolInvertSecondThenAnd state@(State {_bool = (b1 : b2 : bs)}) = boolTemplate (&&) state {_bool = b1 : not b2 : bs}
instructionBoolInvertSecondThenAnd state = state

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

instructionBoolPop :: State -> State
instructionBoolPop state = instructionPop state bool

instructionBoolDup :: State -> State
instructionBoolDup state = instructionDup state bool

instructionBoolDupN :: State -> State
instructionBoolDupN state = instructionDupN state bool

instructionBoolSwap :: State -> State
instructionBoolSwap state = instructionSwap state bool

instructionBoolRot :: State -> State
instructionBoolRot state = instructionRot state bool

instructionBoolFlush :: State -> State
instructionBoolFlush state = instructionFlush state bool

instructionBoolEq :: State -> State
instructionBoolEq state = instructionEq state bool

instructionBoolStackDepth :: State -> State
instructionBoolStackDepth state = instructionStackDepth state bool

instructionBoolYank :: State -> State
instructionBoolYank state = instructionYank state bool

instructionBoolYankDup :: State -> State
instructionBoolYankDup state = instructionYankDup state bool

instructionBoolShove :: State -> State
instructionBoolShove state = instructionShove state bool

instructionBoolShoveDup :: State -> State
instructionBoolShoveDup state = instructionShoveDup state bool

instructionBoolIsEmpty :: State -> State
instructionBoolIsEmpty state = instructionIsEmpty state bool
