module Instructions.GenericInstructions where

import Control.Lens
import State

-- import Debug.Trace 

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs <> drop 1 (drop idx xs)

combineTuple :: a -> ([a], [a]) -> [a]
combineTuple val tup = fst tup <> [val] <> snd tup

combineTupleList :: [a] -> ([a], [a]) -> [a]
combineTupleList val tup = fst tup <> val <> snd tup

insertAt :: Int -> a -> [a] -> [a]
insertAt idx val xs = combineTuple val (splitAt idx xs)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = deleteAt (idx + 1) (insertAt idx val xs)

findSubA :: forall a. Eq a => [a] -> [a] -> Int
findSubA fullA subA 
  | length fullA < length subA = -1
  | length fullA == length subA = if fullA == subA then 0 else -1
  | otherwise = findSubA' fullA subA 0
  where
    findSubA' :: [a] -> [a] -> Int -> Int
    findSubA' fA sA subIndex
      | null fA = -1
      | length sA > length fA = -1
      | sA == take (length sA) fA = subIndex
      | otherwise = findSubA' (drop 1 fA) sA (subIndex + 1)

-- The int is the amount of olds to replace with new
-- Just chain findSubA calls lol
-- Nothing means replace all
-- May not be the most efficient method with the findSubA calls
replace :: Eq a => [a] -> [a] -> [a] -> Maybe Int -> [a]
replace fullA old new (Just amt) =
  if findSubA fullA old /= -1 && amt > 0
    then replace (take (findSubA fullA old) fullA <> new <> drop (findSubA fullA old + length old) fullA) old new (Just $ amt - 1)
    else fullA
replace fullA old new Nothing =
  if findSubA fullA old /= -1
    then replace (take (findSubA fullA old) fullA <> new <> drop (findSubA fullA old + length old) fullA) old new Nothing
    else fullA

amtOccurences :: forall a. Eq a => [a] -> [a] -> Int
amtOccurences fullA subA = amtOccurences' fullA subA 0
  where
    amtOccurences' :: [a] -> [a] -> Int -> Int
    amtOccurences' fA sA count =
      if findSubA fA sA /= -1
        then amtOccurences' (replace fA sA mempty (Just 1)) sA (count + 1)
        else count

takeR :: Int -> [a] -> [a]
takeR amt fullA = drop (length fullA - amt) fullA

dropR :: Int -> [a] -> [a]
dropR amt fullA = take (length fullA - amt) fullA

absNum :: Integral a => a -> [b] -> Int
absNum rawNum lst = abs (fromIntegral rawNum) `mod` length lst

notEmptyStack :: State -> Lens' State [a] -> Bool
notEmptyStack state accessor = not . null $ view accessor state

-- This head error should never happen
instructionDup :: State -> Lens' State [a] -> State
instructionDup state accessor =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x,_) -> state & accessor .~ x : view accessor state

instructionPop :: State -> Lens' State [a] -> State
instructionPop state accessor = state & accessor .~ drop 1 (view accessor state)

-- instructionPop :: State -> Lens' State [a] -> State
-- instructionPop state accessor = if notEmptyStack state accessor then instructionPop state accessor else state

-- I might be able to move some of the int stack error checking
-- to the integer call. For now this may be a tad inefficient.
instructionDupN :: State -> Lens' State [a] -> State
instructionDupN state accessor = 
  if notEmptyStack state accessor
  then
    case uncons (view int state)  of
      Just (i1,_) -> instructionDupNHelper i1 accessor (instructionPop state int)
      _ -> state
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
  -- then state & bool .~ (head stackTop == stackTop !! 1) : view bool state & accessor .~ drop 2 (view accessor state)
  then
    case uncons stackTop of
      Nothing -> state
      Just (x1, x2 : _) -> state & bool .~ (x1 == x2) : view bool state & accessor .~ drop 2 (view accessor state)
      Just _ -> state
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

-- int non generic :(
-- Rewrite this eventually?
instructionShoveDup :: State -> Lens' State [a] -> State
instructionShoveDup state@(State {_int = i : is}) accessor =
  case uncons (view accessor state) of
    Just (x,_) -> (state & accessor .~ combineTuple x (splitAt (max 0 (min i (length (view accessor state) - 1))) (view accessor state))) {_int = is}
    _ -> state
instructionShoveDup state@(State {_int = []}) _ = state

-- also also not int generic
instructionShove :: State -> Lens' State [a] -> State
instructionShove state accessor = instructionShoveDup state accessor & accessor .~ drop 1 (view accessor (instructionShoveDup state accessor))

-- not char generic
instructionConcat :: Semigroup a => State -> Lens' State [a] -> State
instructionConcat state accessor =
  case uncons (view accessor state) of
    Just (x1, x2:_) -> droppedState & accessor .~ (x1 <> x2) : view accessor droppedState
    _ -> state
  where
    droppedState :: State
    droppedState = state & accessor .~ drop 2 (view accessor state)

-- evolve fodder???????????
instructionNoOp :: State -> State
instructionNoOp state = state
