module Instructions.CharInstructions where

import State
import Instructions.GenericInstructions

instructionCharConcat :: State -> State
instructionCharConcat state@(State {_char = c1 : c2 : cs, _string = ss}) = state{_char = cs, _string = [c1, c2] : ss}
instructionCharConcat state = state

instructionCharFromFirstChar :: State -> State
instructionCharFromFirstChar state@(State {_char = cs, _string = s1 : ss}) =
  if not $ null s1
    then state {_char = head s1 : cs, _string = ss}
    else state
instructionCharFromFirstChar state = state

instructionCharFromLastChar :: State -> State
instructionCharFromLastChar state@(State {_char = cs, _string = s1 : ss}) =
  if not $ null s1
    then state {_char = last s1 : cs, _string = ss}
    else state
instructionCharFromLastChar state = state

instructionCharFromNthChar :: State -> State
instructionCharFromNthChar state@(State {_char = cs, _string = s1 : ss, _int = i1 : is}) =
  let    
    index = abs i1 `mod` length s1
  in
    state{_char = s1 !! index : cs, _string = ss, _int = is}
instructionCharFromNthChar state = state
