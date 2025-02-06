module HushGP.Instructions.ExecInstructions where

import HushGP.State
import HushGP.Instructions.IntInstructions
import HushGP.Instructions.GenericInstructions

instructionExecIf :: State -> State
instructionExecIf state@(State {_exec = (e1 : e2 : es), _bool = (b : bs)}) =
  if b
    then state {_exec = e1 : es, _bool = bs}
    else state {_exec = e2 : es, _bool = bs}
instructionExecIf state = state

instructionExecDup :: State -> State
instructionExecDup state = instructionDup state exec

instructionExecDupN :: State -> State
instructionExecDupN state = instructionDupN state exec

instructionExecPop :: State -> State
instructionExecPop state = instructionPop state exec

instructionExecSwap :: State -> State
instructionExecSwap state = instructionSwap state exec

instructionExecRot :: State -> State
instructionExecRot state = instructionRot state exec

instructionExecFlush :: State -> State
instructionExecFlush state = instructionFlush state exec

instructionExecEq :: State -> State
instructionExecEq state = instructionEq state exec

instructionExecStackDepth :: State -> State
instructionExecStackDepth state = instructionStackDepth state exec

instructionExecYank :: State -> State
instructionExecYank state = instructionYank state exec

instructionExecYankDup :: State -> State
instructionExecYankDup state = instructionYankDup state exec

instructionExecShove :: State -> State
instructionExecShove state = instructionShove state exec

instructionExecShoveDup :: State -> State
instructionExecShoveDup state = instructionShoveDup state exec

instructionExecIsEmpty :: State -> State
instructionExecIsEmpty state = instructionIsEmpty state exec

execDoRange :: Gene
execDoRange = StateFunc (instructionExecDoRange, "instructionExecDoRange")

instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {_exec = (e1 : es), _int = (i0 : i1 : is)}) =
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
instructionExecDoCount state@(State {_exec = (e : es), _int = (i : is)}) =
  if i < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i - 1, execDoRange, e] : es, _int = is}
instructionExecDoCount state = state

instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {_exec = (e : es), _int = (i : is)}) =
  if i < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i - 1, execDoRange, Block [StateFunc (instructionIntPop, "instructionIntPop"), e]] : es, _int = is}
instructionExecDoTimes state = state

execWhile :: Gene
execWhile = StateFunc (instructionExecWhile, "instructionExecWhile")

instructionExecWhile :: State -> State
instructionExecWhile state@(State {_exec = (_ : es), _bool = []}) =
  state {_exec = es}
instructionExecWhile state@(State {_exec = alles@(e : es), _bool = (b : bs)}) =
  if b
    then state {_exec = e : execWhile : alles, _bool = bs}
    else state {_exec = es}
instructionExecWhile state = state

instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {_exec = alles@(e : _)}) =
  state {_exec = e : execWhile : alles}
instructionExecDoWhile state = state

-- Eats the _boolean no matter what
instructionExecWhen :: State -> State
instructionExecWhen state@(State {_exec = (_ : es), _bool = (b : bs)}) =
  if not b
    then state {_exec = es, _bool = bs}
    else state {_bool = bs}
instructionExecWhen state = state
