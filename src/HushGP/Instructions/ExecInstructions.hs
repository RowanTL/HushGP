module HushGP.Instructions.ExecInstructions where

import HushGP.State
import HushGP.Instructions.IntInstructions
import HushGP.Instructions.GenericInstructions

-- |Removes the second item from the exec stack if the top of the bool stack is True.
-- Removes the first item from the exec stack if the top of the bool stack is False.
instructionExecIf :: State -> State
instructionExecIf state@(State {_exec = e1 : e2 : es, _bool = b1 : bs}) =
  if b1
    then state {_exec = e1 : es, _bool = bs}
    else state {_exec = e2 : es, _bool = bs}
instructionExecIf state = state

-- |Duplicates the top exec instruction (the one after this one on the stack).
instructionExecDup :: State -> State
instructionExecDup = instructionDup exec

-- |Duplicates the top of the exec stack N times based on the top of
-- int stack (the exec instruction after this one).
instructionExecDupN :: State -> State
instructionExecDupN = instructionDupN exec

-- |Pops the top of the exec stack (the one after this on on the stack).
instructionExecPop :: State -> State
instructionExecPop = instructionPop exec

-- |Swaps the top two instructions on the exec stack (the two after this on the exec stack).
instructionExecSwap :: State -> State
instructionExecSwap = instructionSwap exec

-- |Rotates the top three instructions on the exec stack (the three after this on the exec stack).
instructionExecRot :: State -> State
instructionExecRot = instructionRot exec

-- |Sets the exec stack to []. This stops the program.
instructionExecFlush :: State -> State
instructionExecFlush = instructionFlush exec

-- |Checks if the top two exec instructions are True.
instructionExecEq :: State -> State
instructionExecEq = instructionEq exec

-- |Calculates the size of the exec stack and pushes the result to the int stack.
instructionExecStackDepth :: State -> State
instructionExecStackDepth = instructionStackDepth exec

-- |Moves an item from deep within the exec stack to the top of the exec stack based on
-- the top int from the int stack.
instructionExecYank :: State -> State
instructionExecYank = instructionYank exec

-- |Copies an item from deep within the exec stack to the top of the exec stack based on
-- the top int from the int stack.
instructionExecYankDup :: State -> State
instructionExecYankDup = instructionYankDup exec

-- |Moves an item from the top of the shove stack to deep within the shove stack based on
-- the top int from the int stack.
instructionExecShove :: State -> State
instructionExecShove = instructionShove exec

-- |Copies an item from the top of the shove stack to deep within the shove stack based on
-- the top int from the int stack.
instructionExecShoveDup :: State -> State
instructionExecShoveDup = instructionShoveDup exec

-- |If the code stack is empty, pushes True to bool stack, else False.
instructionExecIsStackEmpty :: State -> State
instructionExecIsStackEmpty = instructionIsStackEmpty exec

-- |Evaluates the top item on the exec stack for each step along the range i to j. Both i and j are 
-- taken from the int stack. Differs from code_do_range only in the source of the code and the recursive call.
instructionExecDoRange :: State -> State
instructionExecDoRange state@(State {_exec = e1 : es, _int = i0 : i1 : is}) =
  if increment i0 i1 /= 0
    then state {_exec = e1 : Block [GeneInt (i1 + increment i0 i1), GeneInt i0, StateFunc (instructionExecDoRange, "instructionExecDoRange"), e1] : es, _int = i1 : is}
    else state {_exec = e1 : es, _int = i1 : is}
  where
    increment :: Int -> Int -> Int
    increment destIdx currentIdx
      | currentIdx < destIdx = 1
      | currentIdx > destIdx = -1
      | otherwise = 0
instructionExecDoRange state = state

-- |Evaluates the top item on the exec stack n times, where n comes from the n comes from the top 
-- of the int stack. Differs from code.do*count only in the source of the code and the recursive call.
instructionExecDoCount :: State -> State
instructionExecDoCount state@(State {_exec = e1 : es, _int = i1 : is}) =
  if i1 < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc (instructionExecDoRange, "instructionExecDoRange"), e1] : es, _int = is}
instructionExecDoCount state = state

-- |Evaluates the top item on the code stack n times, where n comes from the n comes from the top of the int stack.
instructionExecDoTimes :: State -> State
instructionExecDoTimes state@(State {_exec = e1 : es, _int = i1 : is}) =
  if i1 < 1
    then state
    else state {_exec = Block [GeneInt 0, GeneInt $ i1 - 1, StateFunc (instructionExecDoRange, "instructionExecDoRange"), Block [StateFunc (instructionIntPop, "instructionIntPop"), e1]] : es, _int = is}
instructionExecDoTimes state = state

-- |Utility: A shorthand for instructionExecWhile
execWhile :: Gene
execWhile = StateFunc (instructionExecWhile, "instructionExecWhile")

-- |Evaluates the top item on the exec stack repeated until the top bool is no longer True.
instructionExecWhile :: State -> State
instructionExecWhile state@(State {_exec = _ : es, _bool = []}) =
  state {_exec = es}
instructionExecWhile state@(State {_exec = alles@(e1 : es), _bool = b1 : bs}) =
  if b1
    then state {_exec = e1 : execWhile : alles, _bool = bs}
    else state {_exec = es}
instructionExecWhile state = state

-- |Evaluates the top item on the exec stack repeated until the top bool is no longer True.
-- Executes at least once.
instructionExecDoWhile :: State -> State
instructionExecDoWhile state@(State {_exec = alles@(e1 : _)}) =
  state {_exec = e1 : execWhile : alles}
instructionExecDoWhile state = state

-- |Pops the next item on the exec stack without evaluating it 
-- if the top bool is False. Otherwise, has no effect.
-- Eats the top bool no matter what.
instructionExecWhen :: State -> State
instructionExecWhen state@(State {_exec = _ : es, _bool = b1 : bs}) =
  if not b1
    then state {_exec = es, _bool = bs}
    else state {_bool = bs}
instructionExecWhen state = state

-- |The K combinator. Deletes the second to top exec item.
instructionExecK :: State -> State
instructionExecK state@(State {_exec = e1 : _ : es}) = state{_exec = e1 : es}
instructionExecK state = state

-- |The S combinator. Takes the top three top exec items, pushes a Block of the second and third instruction,
-- then the third instruction, and then the first instruction.
instructionExecS :: State -> State
instructionExecS state@(State {_exec = e1 : e2 : e3 : es}) = state{_exec = e1 : e3 : Block [e2, e3] : es}
instructionExecS state = state

-- |The Y combinator. Takes the top exec item. Pushes a Block containing the Y combinator instruction and the top exec item.
-- Then pushes that top exec item again.
instructionExecY :: State -> State
instructionExecY state@(State {_exec = e1 : es}) = state{_exec = e1 : Block [StateFunc (instructionExecY, "instructionExecY"), e1] : es}
instructionExecY state = state

-- |Duplicates the top N items of the exec stack based on the top of the int stack.
instructionExecDupItems :: State -> State
instructionExecDupItems = instructionDupItems exec
