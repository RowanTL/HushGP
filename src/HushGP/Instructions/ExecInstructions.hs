module HushGP.Instructions.ExecInstructions where

import HushGP.State
import HushGP.Instructions.IntInstructions
import HushGP.Instructions.GenericInstructions

instructionExecIf :: State -> State
instructionExecIf state@(State {_exec = e1 : e2 : es, _bool = b1 : bs}) =
  if b1
    then state {_exec = e1 : es, _bool = bs}
    else state {_exec = e2 : es, _bool = bs}
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup = instructionDup exec

instructionExecDupN :: State -> State
instructionExecDupN = instructionDupN exec

instructionExecPop :: State -> State
instructionExecPop = instructionPop exec

instructionExecSwap :: State -> State
instructionExecSwap = instructionSwap exec

instructionExecRot :: State -> State
instructionExecRot = instructionRot exec

instructionExecFlush :: State -> State
instructionExecFlush = instructionFlush exec

instructionExecEq :: State -> State
instructionExecEq = instructionEq exec

instructionExecStackDepth :: State -> State
instructionExecStackDepth = instructionStackDepth exec

instructionExecYank :: State -> State
instructionExecYank = instructionYank exec

instructionExecYankDup :: State -> State
instructionExecYankDup = instructionYankDup exec

instructionExecShove :: State -> State
instructionExecShove = instructionShove exec

instructionExecShoveDup :: State -> State
instructionExecShoveDup = instructionShoveDup exec

instructionExecIsStackEmpty :: State -> State
instructionExecIsStackEmpty = instructionIsStackEmpty exec

execDoRange :: Gene
execDoRange = StateFunc (instructionExecDoRange, "instructionExecDoRange")

instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {_exec = e1 : es, _int = i0 : i1 : is}) =
  if increment i0 i1 /= 0
    then state {_exec = e1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, execDoRange, e1] : es, _int = i1 : is}
    else state {_exec = e1 : es, _int = i1 : is}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

instructionExecDoCount :: State -> State
instructionExecDoCount state@(State {_exec = e1 : es, _int = i1 : is}) =
  if i1 < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i1 - 1, execDoRange, e1] : es, _int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {_exec = e1 : es, _int = i1 : is}) =
  if i1 < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i1 - 1, execDoRange, Block [StateFunc (instructionIntPop, "instructionIntPop"), e1]] : es, _int = is}
instructionExecDoTimes state = state

execWhile :: Gene
execWhile = StateFunc (instructionExecWhile, "instructionExecWhile")

instructionExecWhile :: State -> State
instructionExecWhile state@(State {_exec = _ : es, _bool = []}) =
  state {_exec = es}
instructionExecWhile state@(State {_exec = alles@(e1 : es), _bool = b1 : bs}) =
  if b1
    then state {_exec = e1 : execWhile : alles, _bool = bs}
    else state {_exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {_exec = alles@(e1 : _)}) =
  state {_exec = e1 : execWhile : alles}
instructionExecDoWhile state = state

-- Eats the _boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State {_exec = _ : es, _bool = b1 : bs}) =
  if not b1
    then state {_exec = es, _bool = bs}
    else state {_bool = bs}
instructionExecWhen state = state

-- |The K combinator
instructionExecK :: State -> State
instructionExecK state@(State {_exec = e1 : _ : es}) = state{_exec = e1 : es}
instructionExecK state = state

-- |The S combinator
instructionExecS :: State -> State
instructionExecS state@(State {_exec = e1 : e2 : e3 : es}) = state{_exec = e1 : e3 : Block [e2, e3] : es}
instructionExecS state = state

-- |The Y combinator
instructionExecY :: State -> State
instructionExecY state@(State {_exec = e1 : es}) = state{_exec = e1 : Block [StateFunc (instructionExecY, "instructionExecY"), e1] : es}
instructionExecY state = state

instructionExecDupItems :: State -> State
instructionExecDupItems = instructionDupItems exec
