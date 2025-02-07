module HushGP.Instructions.FloatInstructions where

import Data.Fixed (mod')
import HushGP.Instructions.GenericInstructions
import HushGP.State
import Data.Char

instructionFloatFromInt :: State -> State
instructionFloatFromInt state@(State {_float = fs, _int = (i : is)}) = state {_float = (fromIntegral i :: Float) : fs, _int = is}
instructionFloatFromInt state = state

instructionFloatFromBool :: State -> State
instructionFloatFromBool state@(State {_bool = (b : bs), _float = fs}) = state {_bool = bs, _float = (if b then 1.0 else 0.0) : fs}
instructionFloatFromBool state = state

instructionFloatFromChar :: State -> State
instructionFloatFromChar state@(State {_char = c : cs, _float = fs}) = state {_char = cs, _float = (fromIntegral (ord c) :: Float) : fs}
instructionFloatFromChar state = state

instructionFloatFromString :: State -> State
instructionFloatFromString state@(State {_string = s1 : ss, _float = fs}) =
  if all isDigit s1
  then state{_string = ss, _float = read @Float s1 : fs}
  else state
instructionFloatFromString state = state

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

instructionFloatMod :: State -> State
instructionFloatMod state@(State {_float = (f1 : f2 : fs)}) = state {_float = if f1 /= 0 then f2 `mod'` f1 : fs else f1 : f2 : fs}
instructionFloatMod state = state

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
instructionFloatDup state = instructionDup state float

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

instructionFloatYankDup :: State -> State
instructionFloatYankDup state = instructionYankDup state float

instructionFloatYank :: State -> State
instructionFloatYank state = instructionYank state float

instructionFloatShoveDup :: State -> State
instructionFloatShoveDup state = instructionShoveDup state float

instructionFloatShove :: State -> State
instructionFloatShove state = instructionShove state float

instructionFloatIsStackEmpty :: State -> State
instructionFloatIsStackEmpty state = instructionIsStackEmpty state float

instructionFloatSin :: State -> State
instructionFloatSin state@(State {_float = f1 : fs}) = state {_float = sin f1 : fs}
instructionFloatSin state = state

instructionFloatCos :: State -> State
instructionFloatCos state@(State {_float = f1 : fs}) = state {_float = cos f1 : fs}
instructionFloatCos state = state

instructionFloatTan :: State -> State
instructionFloatTan state@(State {_float = f1 : fs}) = state {_float = tan f1 : fs}
instructionFloatTan state = state

instructionFloatDupItems :: State -> State
instructionFloatDupItems = instructionDupItems float
