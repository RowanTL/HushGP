{-# LANGUAGE TemplateHaskell #-}
module HushGP.Instructions.IntInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
import Data.Char
import HushGP.TH

-- |Converts the top float to an int and pushes the result to the int stack.
instructionIntFromFloat :: State -> State
instructionIntFromFloat state@(State {_float = f1 : fs, _int = is}) = state {_float = fs, _int = floor f1 : is}
instructionIntFromFloat state = state

-- |If the top bool True, pushes 1 to the int stack. Pushes 0 if False.
instructionIntFromBool :: State -> State
instructionIntFromBool state@(State {_bool = b1 : bs, _int = is}) = state {_bool = bs, _int = (if b1 then 1 else 0) : is}
instructionIntFromBool state = state

-- |Takes the top char and converts it to int representation. The result is pushed to the int stack.
instructionIntFromChar :: State -> State
instructionIntFromChar state@(State {_char = c1 : cs, _int = is}) = state {_char = cs, _int = fromIntegral (ord c1) : is}
instructionIntFromChar state = state

-- |Reads the top string and converts it to a int if possible. If not, acts as a NoOp.
instructionIntFromString :: State -> State
instructionIntFromString state@(State {_string = s1 : ss, _int = is}) =
  if all isDigit s1
  then state{_string = ss, _int = read @Integer s1 : is}
  else state
instructionIntFromString state = state

-- |Adds the top two ints from the int stack and pushes the result to the int stack.
instructionIntAdd :: State -> State
instructionIntAdd state@(State {_int = i1 : i2 : is}) = state {_int = i2 + i1 : is}
instructionIntAdd state = state

-- |Subtracts the first int from the second int and pushes the result to the int stack.
instructionIntSub :: State -> State
instructionIntSub state@(State {_int = i1 : i2 : is}) = state {_int = i2 - i1 : is}
instructionIntSub state = state

-- |Subtracts the second int from the first int and pushes the result to the int stack.
instructionIntSubOpp :: State -> State
instructionIntSubOpp state@(State {_int = i1 : i2 : is}) = state {_int = i1 - i2 : is}
instructionIntSubOpp state = state

-- |Multiplies the top two ints from the int stack and pushes the result to the int stack.
instructionIntMul :: State -> State
instructionIntMul state@(State {_int = i1 : i2 : is}) = state {_int = i2 * i1 : is}
instructionIntMul state = state

-- |Divides the first float from the second float and pushes the result to the int stack.
-- This does truncate.
instructionIntDiv :: State -> State
instructionIntDiv state@(State {_int = i1 : i2 : is}) = state {_int = if i1 /= 0 then (i2 `div` i1) : is else i1 : i2 : is}
instructionIntDiv state = state

-- |Divides the second int from the first int and pushes the result to the int stack.
-- This does truncate.
instructionIntDivOpp :: State -> State
instructionIntDivOpp state@(State {_int = i1 : i2 : is}) = state {_int = if i2 /= 0 then (i1 `div` i2) : is else i1 : i2 : is}
instructionIntDivOpp state = state

-- |Mods the first int from the second int and pushes the result to the int stack.
-- This does truncate.
instructionIntMod :: State -> State
instructionIntMod state@(State {_int = i1 : i2 : is}) = state {_int = if i1 /= 0 then (i2 `mod` i1) : is else i1 : i2 : is}
instructionIntMod state = state

-- |Mods the second int from the first int and pushes the result to the int stack.
-- This does truncate.
instructionIntModOpp :: State -> State
instructionIntModOpp state@(State {_int = i1 : i2 : is}) = state {_int = if i2 /= 0 then (i1 `mod` i2) : is else i1 : i2 : is}
instructionIntModOpp state = state

-- |Takes the top two ints from the int stack and pushes the minimum of the two back on top.
instructionIntMin :: State -> State
instructionIntMin state@(State {_int = i1 : i2 : is}) = state {_int = min i1 i2 : is}
instructionIntMin state = state

-- |Takes the top two ints from the int stack and pushes the maximum of the two back on top.
instructionIntMax :: State -> State
instructionIntMax state@(State {_int = i1 : i2 : is}) = state {_int = max i1 i2 : is}
instructionIntMax state = state

-- |Adds one to the top of the int stack and pushes the result back to the int stack.
instructionIntInc :: State -> State
instructionIntInc state@(State {_int = i1 : is}) = state {_int = i1 + 1 : is}
instructionIntInc state = state

-- |Subtracts one from the top of the int stack and pushes the result back to the int stack.
instructionIntDec :: State -> State
instructionIntDec state@(State {_int = i1 : is}) = state {_int = i1 - 1 : is}
instructionIntDec state = state

-- |Takes the top two ints from the int stack and pushes the result of: the top int item < the second int item
instructionIntLT :: State -> State
instructionIntLT state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 < i2) : bs}
instructionIntLT state = state

-- |Takes the top two ints from the int stack and pushes the result of: the top int item > the second int item
instructionIntGT :: State -> State
instructionIntGT state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 > i2) : bs}
instructionIntGT state = state

-- |Takes the top two ints from the int stack and pushes the result of: the top int item <= the second int item
instructionIntLTE :: State -> State
instructionIntLTE state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 <= i2) : bs}
instructionIntLTE state = state

-- |Takes the top two ints from the int stack and pushes the result of: the top int item >= the second int item
instructionIntGTE :: State -> State
instructionIntGTE state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 >= i2) : bs}
instructionIntGTE state = state

-- |Pops the top int from the int stack.
instructionIntDup :: State -> State
instructionIntDup = instructionDup int

-- |Duplicates the top int on the int stack.
instructionIntPop :: State -> State
instructionIntPop = instructionPop int

-- |Duplicates the second to top int on the int stack based on the top int
-- and pushes the result to the int stack.
instructionIntDupN :: State -> State
instructionIntDupN = instructionDupN int

-- |Swaps the top two ints on the int stack.
instructionIntSwap :: State -> State
instructionIntSwap = instructionSwap int

-- |Rotates the top three ints and pushes the result to the int stack.
instructionIntRot :: State -> State
instructionIntRot = instructionRot int

-- |Sets the int stack to [].
instructionIntFlush :: State -> State
instructionIntFlush = instructionFlush int

-- |Checks if the top two floats are equal
instructionIntEq :: State -> State
instructionIntEq = instructionEq int

-- |Pushes the depth of the int stack to top of the int stack after the caluculation.
instructionIntStackDepth :: State -> State
instructionIntStackDepth = instructionStackDepth int

-- |Moves an item from deep within the int stack to the top of the int stack based on
-- the top int from the int stack.
instructionIntYank :: State -> State
instructionIntYank = instructionYank int

-- |Copies an item from deep within the float stack to the top of the float stack based on
-- the top int from the int stack.
instructionIntYankDup :: State -> State
instructionIntYankDup = instructionYankDup int

-- |Moves an item from the top of the int stack to deep within the int stack based on
-- the top int from the int stack.
instructionIntShove :: State -> State
instructionIntShove = instructionShove int

-- |Copies an item from the top of the int stack to deep within the int stack based on
-- the top int from the int stack.
instructionIntShoveDup :: State -> State
instructionIntShoveDup = instructionShoveDup int

-- |Pushes True to the bool stack if the int stack is empty. False if not.
instructionIntIsStackEmpty :: State -> State
instructionIntIsStackEmpty = instructionIsStackEmpty int

-- |Duplicate the top N items from the int stack based on the top int from the int stack.
instructionIntDupItems :: State -> State
instructionIntDupItems = instructionDupItems int

-- |Pushes the sin of the top int to the int stack. Rounding if needed.
instructionIntSin :: State -> State
instructionIntSin state@(State {_int = i1 : is}) = state {_int = round (sin (fromIntegral @Integer @Double i1)) : is}
instructionIntSin state = state

-- |Pushes the cos of the top int to the int stack. Rounding if needed.
instructionIntCos :: State -> State
instructionIntCos state@(State {_int = i1 : is}) = state {_int = round (cos (fromIntegral @Integer @Double i1)) : is}
instructionIntCos state = state

-- |Pushes the tan of the top int to the int stack. Rounding if needed.
instructionIntTan :: State -> State
instructionIntTan state@(State {_int = i1 : is}) = state {_int = round (tan (fromIntegral @Integer @Double i1)) : is}
instructionIntTan state = state

-- |Pushes the absolute value of the top int to the int stack.
instructionIntAbs :: State -> State
instructionIntAbs state@(State {_int = i1 : is}) = state {_int = abs i1 : is}
instructionIntAbs state = state

-- |Pushes the exponential of the top int to the int stack. Rounding if needed.
instructionIntExp :: State -> State
instructionIntExp state@(State {_int = i1 : is}) = state {_int = round (exp (fromIntegral @Integer @Double i1)) : is}
instructionIntExp state = state

-- |Pushes the log of the top int to the int stack. Rounding if needed.
instructionIntLog :: State -> State
instructionIntLog state@(State {_int = i1 : is}) = state {_int = round (log (fromIntegral @Integer @Double i1)) : is}
instructionIntLog state = state

-- |Pushes the squared value of the top int to the int stack.
instructionIntSquare :: State -> State
instructionIntSquare state@(State {_int = i1 : is}) = state {_int = i1 ^ (2 :: Int) : is}
instructionIntSquare state = state

-- |Pushes the cubed value of the top int to the int stack.
instructionIntCube :: State -> State
instructionIntCube state@(State {_int = i1 : is}) = state {_int = i1 ^ (3 :: Int) : is}
instructionIntCube state = state

-- |Pushes the square rooted value of the top int to the int stack. Rounding if needed.
instructionIntSqrt :: State -> State
instructionIntSqrt state@(State {_int = i1 : is}) = state {_int = round (sqrt (fromIntegral @Integer @Double i1)) : is}
instructionIntSqrt state = state

-- |Pushes the top int with its sign reversed to the top of the int stack.
instructionIntReverseSign :: State -> State
instructionIntReverseSign state@(State {_int = i1 : is}) = state {_int = (-1) * i1 : is}
instructionIntReverseSign state = state

allIntInstructions :: [Gene]
allIntInstructions = map StateFunc ($(functionExtractor "instruction"))
