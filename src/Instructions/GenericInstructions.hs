module Instructions.GenericInstructions where

import Control.Lens
import State

-- import Debug.Trace 

-- Files in the spaces in [[a]] with [a]
fillInHoles :: [a] -> [[a]] -> [a]
fillInHoles filler toFill = undefined -- TODO

notEmptyStack :: State -> Lens' State [a] -> Bool
notEmptyStack state accessor = not . null $ view accessor state

-- This head error should never happen
instructionDup :: State -> Lens' State [a] -> State
instructionDup state accessor = if notEmptyStack state accessor then state & accessor .~ head (view accessor state) : view accessor state else state

instructionPop :: State -> Lens' State [a] -> State
instructionPop state accessor = if notEmptyStack state accessor then state & accessor .~ drop 1 (view accessor state) else state

-- I might be able to move some of the int stack error checking
-- to the integer call. For now this may be a tad inefficient.
instructionDupN :: State -> Lens' State [a] -> State
instructionDupN state accessor = 
  if notEmptyStack state accessor && notEmptyStack state int
  then instructionDupNHelper (head (view int state)) accessor (instructionPop state int)
  else state
  where
    instructionDupNHelper :: Int -> Lens' State [a] -> State -> State
    instructionDupNHelper count internalAccessor internalState =
      if count > 1 && notEmptyStack internalState int
      then instructionDupNHelper (count - 1) internalAccessor (instructionDup internalState accessor) 
      else internalState

instructionSwap :: State -> Lens' State [a] -> State
instructionSwap state accessor =
  if (length . take 2 $ view accessor state) == 2
  then state & accessor .~ swapper (view accessor state)
  else state
  where
    swapper :: [a] -> [a]
    swapper (x1 : x2 : xs) = x2 : x1 : xs
    swapper xs = xs

-- Rotates top 3 integers
-- We could use template haskell to rotate any number of these as
-- an instruction later. Template haskell seems very complicated tho.
instructionRot :: State -> Lens' State [a] -> State
instructionRot state accessor =
  if (length . take 3 $ view accessor state) == 3
  then state & accessor .~ rotator (view accessor state)
  else state
  where
    rotator :: [a] -> [a]
    rotator (x1 : x2 : x3 : xs) = x3 : x1 : x2 : xs
    rotator xs = xs

instructionFlush :: State -> Lens' State [a] -> State
instructionFlush state accessor = state & accessor .~ []

instructionEq :: forall a. Eq a => State -> Lens' State [a] -> State
instructionEq state accessor =
  if length stackTop == 2
  then state & bool .~ (head stackTop == stackTop !! 1) : view bool state & accessor .~ drop 2 (view accessor state)
  else state
  where
    stackTop :: [a]
    stackTop = take 2 $ view accessor state

instructionStackDepth :: State -> Lens' State [a] -> State
instructionStackDepth state accessor = state & int .~ (length (view accessor state) : view int state)

-- Will have a non-generic definition for the int stack
instructionYankDup :: State -> Lens' State [a] -> State
instructionYankDup state@(State {_int = i : is}) accessor = 
  if notEmptyStack state accessor
  then (state & accessor .~ (view accessor state !! max 0 (min i (length (view accessor state) - 1))) : view accessor state) {_int = is}
  else state
instructionYankDup state@(State {_int = []}) _ = state

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs <> drop 1 (drop idx xs)

-- Is this optimal? Running instrucitonYankDup twice?????
-- int non generic too
instructionYank :: forall a. State -> Lens' State [a] -> State
instructionYank state@(State {_int = rawIndex : _}) accessor =
  let
    myIndex :: Int
    myIndex = max 0 (min rawIndex (length (view accessor state) - 1))
    item :: a
    item = view accessor state !! myIndex
    deletedState :: State
    deletedState = state & accessor .~ deleteAt myIndex (view accessor state)
  in
  if notEmptyStack state accessor then deletedState & accessor .~ item : view accessor deletedState else state
instructionYank state _ = state

combineTuple :: a -> ([a], [a]) -> [a]
combineTuple val tup = fst tup <> [val] <> snd tup

-- int non generic :(
-- Rewrite this eventually?
instructionShoveDup :: State -> Lens' State [a] -> State
instructionShoveDup state@(State {_int = i : is}) accessor =
  if notEmptyStack state accessor
  then (state & accessor .~ combineTuple (head $ view accessor state) (splitAt (max 0 (min i (length (view accessor state) - 1))) (view accessor state))) {_int = is}
  else state
instructionShoveDup state@(State {_int = []}) _ = state

-- also also not int generic
instructionShove :: State -> Lens' State [a] -> State
instructionShove state accessor = instructionShoveDup state accessor & accessor .~ drop 1 (view accessor (instructionShoveDup state accessor))

-- not char generic
instructionConcat :: Semigroup a => State -> Lens' State [a] -> State
instructionConcat state accessor =
  if (length . take 2 $ view accessor state) == 2
  then droppedState & accessor .~ (head (view accessor state) <> view accessor state !! 1) : view accessor droppedState
  -- then undefined
  else state
  where
    droppedState :: State
    droppedState = state & accessor .~ drop 2 (view accessor state)

-- evolve fodder???????????
instructionNoOp :: State -> State
instructionNoOp state = state
