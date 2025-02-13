{-# LANGUAGE TemplateHaskell #-}
module HushGP.Instructions.FloatInstructions where

import Data.Fixed (mod')
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.Utility
import HushGP.State
import Data.Char
import HushGP.TH

-- |Converts the top int to a float and pushes the result to the float stack.
instructionFloatFromInt :: State -> State
instructionFloatFromInt state@(State {_float = fs, _int = i1 : is}) = state {_float = (fromIntegral i1 :: Float) : fs, _int = is}
instructionFloatFromInt state = state

-- |If the top bool True, pushes 1.0 to the float stack. Pushes 0.0 if False.
instructionFloatFromBool :: State -> State
instructionFloatFromBool state@(State {_bool = b1 : bs, _float = fs}) = state {_bool = bs, _float = (if b1 then 1.0 else 0.0) : fs}
instructionFloatFromBool state = state

-- |Takes the top char and converts it to int representation. That int then gets casted to a float and pushed to the float stack.
instructionFloatFromChar :: State -> State
instructionFloatFromChar state@(State {_char = c1 : cs, _float = fs}) = state {_char = cs, _float = (fromIntegral (ord c1) :: Float) : fs}
instructionFloatFromChar state = state

-- |Reads the top string and converts it to a float if possible. If not, acts as a NoOp.
instructionFloatFromString :: State -> State
instructionFloatFromString state@(State {_string = s1 : ss, _float = fs}) =
  if all (\x -> isDigit x || x == '.') s1 && amtOccurences "." s1 <= 1
  then state{_string = ss, _float = read @Float s1 : fs}
  else state
instructionFloatFromString state = state

-- |Adds the top two floats from the float stack.
instructionFloatAdd :: State -> State
instructionFloatAdd state@(State {_float = f1 : f2 : fs}) = state {_float = f2 + f1 : fs}
instructionFloatAdd state = state

-- |Subtracts the first float from the second float on the float stack.
instructionFloatSub :: State -> State
instructionFloatSub state@(State {_float = f1 : f2 : fs}) = state {_float = f2 - f1 : fs}
instructionFloatSub state = state

-- |Subtracts the second float from the first float and pushes the result to the float stack.
instructionFloatSubOpp :: State -> State
instructionFloatSubOpp state@(State {_float = i1 : i2 : is}) = state {_float = i1 - i2 : is}
instructionFloatSubOpp state = state

-- |Multiplies the top two floats on the float stack.
instructionFloatMul :: State -> State
instructionFloatMul state@(State {_float = f1 : f2 : fs}) = state {_float = f2 * f1 : fs}
instructionFloatMul state = state

-- |Divides the first float from the second float on the float stack.
instructionFloatDiv :: State -> State
instructionFloatDiv state@(State {_float = f1 : f2 : fs}) = state {_float = if f1 /= 0 then f2 / f1 : fs else f1 : f2 : fs}
instructionFloatDiv state = state

-- |Divides the second float from the first float and pushes the result to the float stack.
-- This does truncate.
instructionFloatDivOpp :: State -> State
instructionFloatDivOpp state@(State {_float = i1 : i2 : is}) = state {_float = if i2 /= 0 then (i1 / i2) : is else i1 : i2 : is}
instructionFloatDivOpp state = state

-- |Mods the first float from the second float on the float stack.
instructionFloatMod :: State -> State
instructionFloatMod state@(State {_float = f1 : f2 : fs}) = state {_float = if f1 /= 0 then f2 `mod'` f1 : fs else f1 : f2 : fs}
instructionFloatMod state = state

-- |Takes the top two floats from the float stack and pushes the minimum of the two back on top.
instructionFloatMin :: State -> State
instructionFloatMin state@(State {_float = f1 : f2 : fs}) = state {_float = min f1 f2 : fs}
instructionFloatMin state = state

-- |Takes the top two floats from the float stack and pushes the maximum of the two back on top.
instructionFloatMax :: State -> State
instructionFloatMax state@(State {_float = f1 : f2 : fs}) = state {_float = max f1 f2 : fs}
instructionFloatMax state = state

-- |Adds one to the top float from the float stack.
instructionFloatInc :: State -> State
instructionFloatInc state@(State {_float = f1 : fs}) = state {_float = f1 + 1 : fs}
instructionFloatInc state = state

-- |Subtracts one from the top float from the float stack.
instructionFloatDec :: State -> State
instructionFloatDec state@(State {_float = f1 : fs}) = state {_float = f1 - 1 : fs}
instructionFloatDec state = state

-- |Takes the top two floats from the float stack and pushes the result of: the top float item < the second float item
instructionFloatLT :: State -> State
instructionFloatLT state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 < f2) : bs}
instructionFloatLT state = state

-- |Takes the top two floats from the float stack and pushes the result of: the top float item > the second float item
instructionFloatGT :: State -> State
instructionFloatGT state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 > f2) : bs}
instructionFloatGT state = state

-- |Takes the top two floats from the float stack and pushes the result of: the top float item <= the second float item
instructionFloatLTE :: State -> State
instructionFloatLTE state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 <= f2) : bs}
instructionFloatLTE state = state

-- |Takes the top two floats from the float stack and pushes the result of: the top float item >= the second float item
instructionFloatGTE :: State -> State
instructionFloatGTE state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 >= f2) : bs}
instructionFloatGTE state = state

-- |Pops the top float from the float stack.
instructionFloatPop :: State -> State
instructionFloatPop = instructionPop float

-- |Duplicates the top float on the float stack.
instructionFloatDup :: State -> State
instructionFloatDup = instructionDup float

-- |Duplicates the top float on the float stack N times based off the top of the int stack.
instructionFloatDupN :: State -> State
instructionFloatDupN = instructionDupN float

-- |Swaps the top two floats on the float stack.
instructionFloatSwap :: State -> State
instructionFloatSwap = instructionSwap float

-- |Rotates the top three floats on the float stack.
instructionFloatRot :: State -> State
instructionFloatRot = instructionRot float

-- |Sets the float stack to []
instructionFloatFlush :: State -> State
instructionFloatFlush = instructionFlush float

-- |Checks if the top two floats are equal. Pushes the result to the bool stack.
-- Might override this later to check for equality in a range rather than exact equality.
instructionFloatEq :: State -> State
instructionFloatEq = instructionEq float

-- |Pushes the depth of the stack to the int stack.
instructionFloatStackDepth :: State -> State
instructionFloatStackDepth = instructionStackDepth float

-- |Copies an item from deep within the float stack to the top of the float stack based on
-- the top int from the int stack.
instructionFloatYankDup :: State -> State
instructionFloatYankDup = instructionYankDup float

-- |Moves an item from deep within the float stack to the top of the float stack based on
-- the top int from the int stack.
instructionFloatYank :: State -> State
instructionFloatYank = instructionYank float

-- |Copies an item from the top of the float stack to deep within the float stack based on
-- the top int from the int stack.
instructionFloatShoveDup :: State -> State
instructionFloatShoveDup = instructionShoveDup float

-- |Moves an item from the top of the float stack to deep within the float stack based on
-- the top int from the int stack.
instructionFloatShove :: State -> State
instructionFloatShove = instructionShove float

-- |Pushes True to the bool stack if the float stack is empty. False if not.
instructionFloatIsStackEmpty :: State -> State
instructionFloatIsStackEmpty = instructionIsStackEmpty float

-- |Pushes the sin of the top float to the float stack.
instructionFloatSin :: State -> State
instructionFloatSin state@(State {_float = f1 : fs}) = state {_float = sin f1 : fs}
instructionFloatSin state = state

-- |Pushes the cos of the top float to the float stack.
instructionFloatCos :: State -> State
instructionFloatCos state@(State {_float = f1 : fs}) = state {_float = cos f1 : fs}
instructionFloatCos state = state

-- |Pushes the tan of the top float to the float stack.
instructionFloatTan :: State -> State
instructionFloatTan state@(State {_float = f1 : fs}) = state {_float = tan f1 : fs}
instructionFloatTan state = state

-- |Duplicate the top N items from the float stack based on the top int from the int stack.
instructionFloatDupItems :: State -> State
instructionFloatDupItems = instructionDupItems float

allFloatInstructions :: [Gene]
allFloatInstructions = map StateFunc ($(functionExtractor "instruction"))
