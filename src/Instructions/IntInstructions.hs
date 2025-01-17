module Instructions.IntInstructions where

import State
import Debug.Trace

instructionIntAdd :: State -> State
instructionIntAdd state@(State {int = (i1 : i2 : is)}) = state {int = i2 + i1 : is}
instructionIntAdd state = state

instructionIntSub :: State -> State
instructionIntSub state@(State {int = (i1 : i2 : is)}) = state {int = i2 - i1 : is}
instructionIntSub state = state

instructionIntMul :: State -> State
instructionIntMul state@(State {int = (i1 : i2 : is)}) = state {int = i2 * i1 : is}
instructionIntMul state = state

instructionIntDiv :: State -> State
instructionIntDiv state@(State {int = (i1 : i2 : is)}) = state {int = if i1 /= 0 then (i2 `div` i1) : is else i1 : i2 : is}
instructionIntDiv state = state

instructionIntMod :: State -> State
instructionIntMod state@(State {int = (i1 : i2 : is)}) = state {int = i2 `mod` i1 : is}
instructionIntMod state = state

instructionIntMin :: State -> State
instructionIntMin state@(State {int = (i1 : i2 : is)}) = state {int = min i1 i2 : is}
instructionIntMin state = state

instructionIntMax :: State -> State
instructionIntMax state@(State {int = (i1 : i2 : is)}) = state {int = max i1 i2 : is}
instructionIntMax state = state

instructionIntInc :: State -> State
instructionIntInc state@(State {int = (i1 : is)}) = state {int = i1 + 1 : is}
instructionIntInc state = state

instructionIntDec :: State -> State
instructionIntDec state@(State {int = (i1 : is)}) = state {int = i1 - 1 : is}
instructionIntDec state = state

instructionIntLT :: State -> State
instructionIntLT state@(State {int = i1 : i2 : is, bool = bs}) = state {int = is, bool = (i1 < i2) : bs}
instructionIntLT state = state

instructionIntGT :: State -> State
instructionIntGT state@(State {int = i1 : i2 : is, bool = bs}) = state {int = is, bool = (i1 > i2) : bs}
instructionIntGT state = state

instructionIntLTE :: State -> State
instructionIntLTE state@(State {int = i1 : i2 : is, bool = bs}) = state {int = is, bool = (i1 <= i2) : bs}
instructionIntLTE state = state

instructionIntGTE :: State -> State
instructionIntGTE state@(State {int = i1 : i2 : is, bool = bs}) = state {int = is, bool = (i1 >= i2) : bs}
instructionIntGTE state = state

instructionIntPop :: State -> State
instructionIntPop state@(State {int = (_ : is)}) = state {int = is}
instructionIntPop state = state
