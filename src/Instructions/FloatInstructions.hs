module Instructions.FloatInstructions where

import State

instructionFloatAdd :: State -> State
instructionFloatAdd state@(State {float = (f1 : f2 : fs)}) = state {float = f2 + f1 : fs}
instructionFloatAdd state = state

instructionFloatSub :: State -> State
instructionFloatSub state@(State {float = (f1 : f2 : fs)}) = state {float = f2 - f1 : fs}
instructionFloatSub state = state

instructionFloatMul :: State -> State
instructionFloatMul state@(State {float = (f1 : f2 : fs)}) = state {float = f2 * f1 : fs}
instructionFloatMul state = state

instructionFloatDiv :: State -> State
instructionFloatDiv state@(State {float = (f1 : f2 : fs)}) = state {float = if f1 /= 0 then f2 / f1 : fs else f1 : f2 : fs}
instructionFloatDiv state = state

instructionFloatMin :: State -> State
instructionFloatMin state@(State {float = (f1 : f2 : fs)}) = state {float = min f1 f2 : fs}
instructionFloatMin state = state

instructionFloatMax :: State -> State
instructionFloatMax state@(State {float = (f1 : f2 : fs)}) = state {float = max f1 f2 : fs}
instructionFloatMax state = state

instructionFloatInc :: State -> State
instructionFloatInc state@(State {float = (f1 : fs)}) = state {float = f1 + 1 : fs}
instructionFloatInc state = state

instructionFloatDec :: State -> State
instructionFloatDec state@(State {float = (f1 : fs)}) = state {float = f1 - 1 : fs}
instructionFloatDec state = state

instructionFloatLT :: State -> State
instructionFloatLT state@(State {float = f1 : f2 : fs, bool = bs}) = state {float = fs, bool = (f1 < f2) : bs}
instructionFloatLT state = state

instructionFloatGT :: State -> State
instructionFloatGT state@(State {float = f1 : f2 : fs, bool = bs}) = state {float = fs, bool = (f1 > f2) : bs}
instructionFloatGT state = state

instructionFloatLTE :: State -> State
instructionFloatLTE state@(State {float = f1 : f2 : fs, bool = bs}) = state {float = fs, bool = (f1 <= f2) : bs}
instructionFloatLTE state = state

instructionFloatGTE :: State -> State
instructionFloatGTE state@(State {float = f1 : f2 : fs, bool = bs}) = state {float = fs, bool = (f1 >= f2) : bs}
instructionFloatGTE state = state

instructionFloatPop :: State -> State
instructionFloatPop state@(State {float = (_ : fs)}) = state {float = fs}
instructionFloatPop state = state

