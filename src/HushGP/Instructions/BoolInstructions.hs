module HushGP.Instructions.BoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

-- |If top of int stack /= 0 pushes True to bool stack, else false.
instructionBoolFromInt :: State -> State
instructionBoolFromInt state@(State {_int = i1 : is, _bool = bs}) = state {_int = is, _bool = (i1 /= 0) : bs}
instructionBoolFromInt state = state

-- |If top of float stack /= 0 pushes True to bool stack, else false.
instructionBoolFromFloat :: State -> State
instructionBoolFromFloat state@(State {_float = f1 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 /= 0) : bs}
instructionBoolFromFloat state = state

-- |A template function to make bool comparisons concise.
boolTemplate :: (Bool -> Bool -> Bool) -> State -> State
boolTemplate func state@(State {_bool = b1 : b2 : bs}) = state {_bool = func b1 b2 : bs}
boolTemplate _ state = state

-- |Takes the top two bools and Ands them.
instructionBoolAnd :: State -> State
instructionBoolAnd = boolTemplate (&&)

-- |Takes the top two bools, inverts the first bool and then Ands the modified state.
instructionBoolInvertFirstThenAnd :: State -> State
instructionBoolInvertFirstThenAnd state@(State {_bool = b1 : bs}) = boolTemplate (&&) state {_bool = not b1 : bs}
instructionBoolInvertFirstThenAnd state = state

-- |Takes the top two bools, inverts the second bool and then Ands the modified state.
instructionBoolInvertSecondThenAnd :: State -> State
instructionBoolInvertSecondThenAnd state@(State {_bool = b1 : b2 : bs}) = boolTemplate (&&) state {_bool = b1 : not b2 : bs}
instructionBoolInvertSecondThenAnd state = state

-- |Takes the top two bools and Ors them.
instructionBoolOr :: State -> State
instructionBoolOr = boolTemplate (||)

-- |Utility function. Haskell doesn't have its own xor operation.
xor :: Bool -> Bool -> Bool
xor b1 b2
  | b1 && not b2 = True
  | not b1 && b2 = True
  | otherwise = False

-- |Takes the xor of the top two bools.
instructionBoolXor :: State -> State
instructionBoolXor = boolTemplate xor

-- |Pops the top of the bool stack.
instructionBoolPop :: State -> State
instructionBoolPop = instructionPop bool

-- |Duplicates the top of the bool stack.
instructionBoolDup :: State -> State
instructionBoolDup = instructionDup bool

-- |Duplicates the top of the bool stack based on the top int from the int stack.
instructionBoolDupN :: State -> State
instructionBoolDupN = instructionDupN bool

-- |Swaps the top two bools.
instructionBoolSwap :: State -> State
instructionBoolSwap = instructionSwap bool

-- |Rotates the top three bools.
instructionBoolRot :: State -> State
instructionBoolRot = instructionRot bool

-- |Sets the bool stack to []
instructionBoolFlush :: State -> State
instructionBoolFlush = instructionFlush bool

-- |Tests if the top two bools are equal and pushes the result to the bool stack.
instructionBoolEq :: State -> State
instructionBoolEq = instructionEq bool

-- |Calculates the size of a stack and pushes the result to the int stack.
instructionBoolStackDepth :: State -> State
instructionBoolStackDepth = instructionStackDepth bool

-- |Moves an item from deep within the bool stack to the top of the bool stack based on
-- the top int from the int stack
instructionBoolYank :: State -> State
instructionBoolYank = instructionYank bool

-- |Copies an item from deep within the bool stack to the top of the bool stack based on
-- the top int from the int stack.
instructionBoolYankDup :: State -> State
instructionBoolYankDup = instructionYankDup bool

-- |Moves an item from the top of the bool stack to deep within the bool stack based on
-- the top int from the int stack.
instructionBoolShove :: State -> State
instructionBoolShove = instructionShove bool

-- |Copies an item from the top of the bool stack to deep within the bool stack based on
-- the top int from the int stack.
instructionBoolShoveDup :: State -> State
instructionBoolShoveDup = instructionShoveDup bool

-- |If the bool stack is empty, pushes True to bool stack, else False.
instructionBoolIsStackEmpty :: State -> State
instructionBoolIsStackEmpty = instructionIsStackEmpty bool

-- |Duplicate the top N items from the bool stack based on the top int from the int stack.
instructionBoolDupItems :: State -> State
instructionBoolDupItems = instructionDupItems bool
