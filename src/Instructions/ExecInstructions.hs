module Instructions.ExecInstructions where

import State
import Instructions.IntInstructions

instructionExecIf :: State -> State
instructionExecIf state@(State {_exec = (e1 : e2 : es), _bool = (b : _)}) =
  if b
    then state {_exec = e1 : es}
    else state {_exec = e2 : es}
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup state@(State {_exec = alles@(e : _)}) =
  state {_exec = e : alles}
instructionExecDup state = state

instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {_exec = (e1 : es), _int = (i0 : i1 : is)}) =
  if increment i0 i1 /= 0
    then state {_exec = e1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc instructionExecDoRange, e1] : es, _int = i1 : is}
    else state {_exec = e1 : es, _int = i1 : is}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

instructionExecDoCount :: State -> State
instructionExecDoCount state@(State {_exec = (e : es), _int = (i : is)}) =
  if i < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i - 1, StateFunc instructionExecDoRange, e] : es, _int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {_exec = (e : es), _int = (i : is)}) =
  if i < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i - 1, StateFunc instructionExecDoRange, Block [StateFunc instructionIntPop, e]] : es, _int = is}
instructionExecDoTimes state = state

instructionExecWhile :: State -> State
instructionExecWhile state@(State {_exec = (_ : es), _bool = []}) =
  state {_exec = es}
instructionExecWhile state@(State {_exec = alles@(e : es), _bool = (b : bs)}) =
  if b
    then state {_exec = e : StateFunc instructionExecWhile : alles, _bool = bs}
    else state {_exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {_exec = alles@(e : _)}) =
  state {_exec = e : StateFunc instructionExecWhile : alles}
instructionExecDoWhile state = state

-- Eats the _boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State {_exec = (_ : es), _bool = (b : bs)}) =
  if not b
    then state {_exec = es, _bool = bs}
    else state {_bool = bs}
instructionExecWhen state = state
