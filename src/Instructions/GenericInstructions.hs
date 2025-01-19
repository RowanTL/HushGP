module Instructions.GenericInstructions where

import Control.Lens
import State

-- import Debug.Trace

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
