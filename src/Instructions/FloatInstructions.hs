module Instructions.FloatInstructions where

import Instructions.GenericInstructions
import State

instructionFloatAdd :: State -> State
instructionFloatAdd state@(State {_float = (f1 : f2 : fs)}) = state {_float = f2 + f1 : fs}
instructionFloatAdd state = state

instructionFloatSub :: State -> State
instructionFloatSub state@(State {_float = (f1 : f2 : fs)}) = state {_float = f2 - f1 : fs}
instructionFloatSub state = state

instructionFloatMul :: State -> State
instructionFloatMul state@(State {_float = (f1 : f2 : fs)}) = state {_float = f2 * f1 : fs}
instructionFloatMul state = state

instructionFloatDiv :: State -> State
instructionFloatDiv state@(State {_float = (f1 : f2 : fs)}) = state {_float = if f1 /= 0 then f2 / f1 : fs else f1 : f2 : fs}
instructionFloatDiv state = state

instructionFloatMin :: State -> State
instructionFloatMin state@(State {_float = (f1 : f2 : fs)}) = state {_float = min f1 f2 : fs}
instructionFloatMin state = state

instructionFloatMax :: State -> State
instructionFloatMax state@(State {_float = (f1 : f2 : fs)}) = state {_float = max f1 f2 : fs}
instructionFloatMax state = state

instructionFloatInc :: State -> State
instructionFloatInc state@(State {_float = (f1 : fs)}) = state {_float = f1 + 1 : fs}
instructionFloatInc state = state

instructionFloatDec :: State -> State
instructionFloatDec state@(State {_float = (f1 : fs)}) = state {_float = f1 - 1 : fs}
instructionFloatDec state = state

instructionFloatLT :: State -> State
instructionFloatLT state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 < f2) : bs}
instructionFloatLT state = state

instructionFloatGT :: State -> State
instructionFloatGT state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 > f2) : bs}
instructionFloatGT state = state

instructionFloatLTE :: State -> State
instructionFloatLTE state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 <= f2) : bs}
instructionFloatLTE state = state

instructionFloatGTE :: State -> State
instructionFloatGTE state@(State {_float = f1 : f2 : fs, _bool = bs}) = state {_float = fs, _bool = (f1 >= f2) : bs}
instructionFloatGTE state = state

instructionFloatPop :: State -> State
instructionFloatPop state = instructionPop state float

instructionFloatDup :: State -> State
instructionFloatDup state = instructionPop state float

instructionFloatDupN :: State -> State
instructionFloatDupN state = instructionDupN state float

instructionFloatSwap :: State -> State
instructionFloatSwap state = instructionSwap state float

instructionFloatRot :: State -> State
instructionFloatRot state = instructionRot state float

instructionFloatFlush :: State -> State
instructionFloatFlush state = instructionFlush state float

instructionFloatEq :: State -> State
instructionFloatEq state = instructionEq state float

instructionFloatStackDepth :: State -> State
instructionFloatStackDepth state = instructionStackDepth state float
