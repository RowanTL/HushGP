module Instructions.IntInstructions where

import State
import Instructions.GenericInstructions
-- import Debug.Trace

instructionIntFromFloat :: State -> State
instructionIntFromFloat state@(State {_float = (f : fs), _int = is}) = state {_float = fs, _int = floor f : is}
instructionIntFromFloat state = state

instructionIntFromBool :: State -> State
instructionIntFromBool state@(State {_bool = (b : bs), _int = is}) = state {_bool = bs, _int = (if b then 1 else 0) : is}
instructionIntFromBool state = state

instructionIntAdd :: State -> State
instructionIntAdd state@(State {_int = (i1 : i2 : is)}) = state {_int = i2 + i1 : is}
instructionIntAdd state = state

instructionIntSub :: State -> State
instructionIntSub state@(State {_int = (i1 : i2 : is)}) = state {_int = i2 - i1 : is}
instructionIntSub state = state

instructionIntMul :: State -> State
instructionIntMul state@(State {_int = (i1 : i2 : is)}) = state {_int = i2 * i1 : is}
instructionIntMul state = state

instructionIntDiv :: State -> State
instructionIntDiv state@(State {_int = (i1 : i2 : is)}) = state {_int = if i1 /= 0 then (i2 `div` i1) : is else i1 : i2 : is}
instructionIntDiv state = state

instructionIntMod :: State -> State
instructionIntMod state@(State {_int = (i1 : i2 : is)}) = state {_int = i2 `mod` i1 : is}
instructionIntMod state = state

instructionIntMin :: State -> State
instructionIntMin state@(State {_int = (i1 : i2 : is)}) = state {_int = min i1 i2 : is}
instructionIntMin state = state

instructionIntMax :: State -> State
instructionIntMax state@(State {_int = (i1 : i2 : is)}) = state {_int = max i1 i2 : is}
instructionIntMax state = state

instructionIntInc :: State -> State
instructionIntInc state@(State {_int = (i1 : is)}) = state {_int = i1 + 1 : is}
instructionIntInc state = state

instructionIntDec :: State -> State
instructionIntDec state@(State {_int = (i1 : is)}) = state {_int = i1 - 1 : is}
instructionIntDec state = state

instructionIntLT :: State -> State
instructionIntLT state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 < i2) : bs}
instructionIntLT state = state

instructionIntGT :: State -> State
instructionIntGT state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 > i2) : bs}
instructionIntGT state = state

instructionIntLTE :: State -> State
instructionIntLTE state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 <= i2) : bs}
instructionIntLTE state = state

instructionIntGTE :: State -> State
instructionIntGTE state@(State {_int = i1 : i2 : is, _bool = bs}) = state {_int = is, _bool = (i1 >= i2) : bs}
instructionIntGTE state = state

instructionIntDup :: State -> State
instructionIntDup state = instructionDup state int

instructionIntPop :: State -> State
instructionIntPop state = instructionPop state int

instructionIntDupN :: State -> State
instructionIntDupN state = instructionDupN state int

instructionIntSwap :: State -> State
instructionIntSwap state = instructionSwap state int

instructionIntRot :: State -> State
instructionIntRot state = instructionRot state int

instructionIntFlush :: State -> State
instructionIntFlush state = instructionFlush state int

instructionIntEq :: State -> State
instructionIntEq state = instructionEq state int

instructionIntStackDepth :: State -> State
instructionIntStackDepth state = instructionStackDepth state int

-- int specific
instructionIntYank :: State -> State
-- instructionIntYank state = instructionYank state int
instructionIntYank state@(State {_int = rawIndex : i1 : is}) = 
  let
    myIndex :: Int
    myIndex = max 0 (min rawIndex (length is - 1))
  in
  state {_int = is !! myIndex : i1 : deleteAt myIndex is}
instructionIntYank state = state

instructionIntYankDup :: State -> State
instructionIntYankDup state@(State {_int = rawIndex : item : is}) = 
  let
    myIndex :: Int
    myIndex = max 0 (min rawIndex (length is - 1))
  in
  state {_int = is !! myIndex : item : is}
instructionIntYankDup state = state

instructionIntShove :: State -> State
instructionIntShove state@(State {_int = rawIndex : item : is}) =
  let
    myIndex :: Int
    myIndex = max 0 (min rawIndex (length is - 1))
  in
  state {_int = combineTuple item (splitAt myIndex is)}
instructionIntShove state = state

instructionIntShoveDup :: State -> State
instructionIntShoveDup state@(State {_int = rawIndex : item : is}) =
  let
    myIndex :: Int
    myIndex = max 0 (min rawIndex (length is - 1))
  in
  state {_int = item : combineTuple item (splitAt myIndex is)}
instructionIntShoveDup state = state

instructionIntIsEmpty :: State -> State
instructionIntIsEmpty state = instructionIsEmpty state int
