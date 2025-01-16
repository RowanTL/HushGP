module Instructions.ExecInstructions where

import State
import Instructions.IntInstructions

instructionExecIf :: State -> State
instructionExecIf state@(State {exec = (e1 : e2 : es), bool = (b : _)}) =
  if b
    then state {exec = e1 : es}
    else state {exec = e2 : es}
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup state@(State {exec = alles@(e0 : _)}) =
  state {exec = e0 : alles}
instructionExecDup state = state

instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {exec = (e1 : es), int = (i0 : i1 : is)}) =
  if increment i0 i1 /= 0
    then state {exec = e1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc instructionExecDoRange, e1] : es, int = i1 : is}
    else state {exec = e1 : es, int = i1 : is}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

instructionExecDoCount :: State -> State
instructionExecDoCount state@(State {exec = (e1 : es), int = (i1 : is)}) =
  if i1 < 1
    then state
    else state {exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc instructionExecDoRange, e1] : es, int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {exec = (e1 : es), int = (i1 : is)}) =
  if i1 < 1
    then state
    else state {exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc instructionExecDoRange, Block [StateFunc instructionIntPop, e1]] : es, int = is}
instructionExecDoTimes state = state

instructionExecWhile :: State -> State
instructionExecWhile state@(State {exec = (_ : es), bool = []}) =
  state {exec = es}
instructionExecWhile state@(State {exec = alles@(e1 : es), bool = (b1 : bs)}) =
  if b1
    then state {exec = e1 : StateFunc instructionExecWhile : alles, bool = bs}
    else state {exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {exec = alles@(e1 : _)}) =
  state {exec = e1 : StateFunc instructionExecWhile : alles}
instructionExecDoWhile state = state

-- Eats the boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State {exec = (_ : es), bool = (b1 : bs)}) =
  if not b1
    then state {exec = es, bool = bs}
    else state {bool = bs}
instructionExecWhen state = state
