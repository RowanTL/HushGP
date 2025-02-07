module HushGP.Instructions.GenericInstructions where

import Control.Lens
import HushGP.State
import Data.List (sort, sortBy)
import Data.Ord
import Data.List.Split

-- import Debug.Trace 

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs <> drop 1 (drop idx xs)

-- I could probably just combine these functions
combineTuple :: a -> ([a], [a]) -> [a]
combineTuple val tup = fst tup <> [val] <> snd tup

combineTupleList :: [a] -> ([a], [a]) -> [a]
combineTupleList val tup = fst tup <> val <> snd tup

insertAt :: Int -> a -> [a] -> [a]
insertAt idx val xs = combineTuple val (splitAt idx xs)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = deleteAt (idx + 1) (insertAt idx val xs)

subList :: Int -> Int -> [a] -> [a]
subList idx0 idx1 xs =
  let
    (start, end) = if idx0 <= idx1 then (idx0, idx1) else (idx1, idx0)
    adjStart = max 0 start
    adjEnd = min end (length xs)
  in
    take adjEnd (drop adjStart xs)

-- Maybe could've used Data.List.isSubsequenceOf :shrug:
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

-- a rather inefficient search
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

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

absNum :: Integral a => a -> [b] -> Int
absNum rawNum lst = abs (fromIntegral rawNum) `mod` length lst

notEmptyStack :: State -> Lens' State [a] -> Bool
notEmptyStack state accessor = not . null $ view accessor state

instructionDup :: State -> Lens' State [a] -> State
instructionDup state accessor =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x,_) -> state & accessor .~ x : view accessor state

instructionPop :: State -> Lens' State [a] -> State
instructionPop state accessor = state & accessor .~ drop 1 (view accessor state)

instructionIsStackEmpty :: State -> Lens' State [a] -> State
instructionIsStackEmpty state@(State {_bool = bs}) accessor = state{_bool = null (view accessor state) : bs}

-- instructionPop :: State -> Lens' State [a] -> State
-- instructionPop state accessor = if notEmptyStack state accessor then instructionPop state accessor else state

-- I might be able to move some of the int stack error checking
-- to the integer call. For now this may be a tad inefficient.
instructionDupN :: forall a. State -> Lens' State [a] -> State
instructionDupN state accessor = 
  case uncons (view int state) of
    Just (i1,is) ->
      case uncons (view accessor state{_int = is}) of
        Just (a1,as) -> 
          instructionDupNHelper i1 a1 accessor (state{_int = is} & accessor .~ as)
        _ -> state
    _ -> state
  where
    instructionDupNHelper :: Int -> a -> Lens' State [a] -> State -> State
    instructionDupNHelper count instruction internalAccessor internalState =
      if count > 0
      then instructionDupNHelper (count - 1) instruction internalAccessor (internalState & accessor .~ (instruction : view accessor internalState))
      else internalState

-- |Duplicates the top N items on a stack. If n <= 0 nothing happens
-- TODO: Will need to implement a max stack items at some point
instructionDupItems :: Lens' State [a] -> State -> State
instructionDupItems accessor state@(State {_int = i1 : is}) =
  if i1 <= 0
  then state{_int = is}
  else state{_int = is} & accessor .~ (take i1 (view accessor state{_int = is}) <> view accessor state{_int = is})
instructionDupItems _ state = state

instructionSwap :: State -> Lens' State [a] -> State
instructionSwap state accessor =
  state & accessor .~ swapper (view accessor state)
  where
    swapper :: [a] -> [a]
    swapper (x1 : x2 : xs) = x2 : x1 : xs
    swapper xs = xs

-- Rotates top 3 integers
-- We could use template haskell to rotate any number of these as
-- an instruction later. Template haskell seems very complicated tho.
instructionRot :: State -> Lens' State [a] -> State
instructionRot state accessor =
  state & accessor .~ rotator (view accessor state)
  where
    rotator :: [a] -> [a]
    rotator (x1 : x2 : x3 : xs) = x3 : x1 : x2 : xs
    rotator xs = xs

instructionFlush :: State -> Lens' State [a] -> State
instructionFlush state accessor = state & accessor .~ []

instructionEq :: forall a. Eq a => State -> Lens' State [a] -> State
instructionEq state accessor =
  case uncons $ view accessor state of
    Nothing -> state
    Just (x1, x2 : _) -> droppedState & bool .~ (x1 == x2) : view bool droppedState
    Just _ -> state
  where
    droppedState :: State
    droppedState = state & accessor .~ drop 2 (view accessor state)

instructionStackDepth :: State -> Lens' State [a] -> State
instructionStackDepth state@(State {_int = is}) accessor = state{_int = length (view accessor state) : is}

instructionYankDup :: State -> Lens' State [a] -> State
instructionYankDup state@(State {_int = i : is}) accessor = 
  if notEmptyStack state accessor
  then state{_int = is} & accessor .~ (view accessor state{_int = is} !! max 0 (min i (length (view accessor state{_int = is}) - 1))) : view accessor state{_int = is}
  else state
instructionYankDup state _ = state

instructionYank :: forall a. State -> Lens' State [a] -> State
instructionYank state@(State {_int = i : is}) accessor =
  let
    myIndex :: Int
    myIndex = max 0 (min i (length (view accessor state{_int = is}) - 1))
    item :: a
    item = view accessor state{_int = is} !! myIndex
    deletedState :: State
    deletedState = state{_int = is} & accessor .~ deleteAt myIndex (view accessor state{_int = is})
  in
  if notEmptyStack state{_int = is} accessor then deletedState & accessor .~ item : view accessor deletedState else state
instructionYank state _ = state

-- In pysh, instructionShoveDup and instructionShove behave differently when indexing in such a way that
-- the duplicated index matters whether or not it's present in the stack at the moment of calculation.
-- I'm not going to keep this behavior. Check out interpysh examples for how pysh handles it.
instructionShoveDup :: State -> Lens' State [a] -> State
instructionShoveDup state@(State {_int = i : is}) accessor =
  case uncons (view accessor state{_int = is}) of
    Just (x,_) -> state{_int = is} & accessor .~ combineTuple x (splitAt (max 0 (min i (length (view accessor state{_int = is}) - 1))) (view accessor state{_int = is}))
    _ -> state
instructionShoveDup state _ = state

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

instructionConj :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionConj state primAccessor vectorAccessor =
  case (uncons (view primAccessor state), uncons (view vectorAccessor state)) of
    (Just (p1,ps), Just (v1,vs)) -> state & primAccessor .~ ps & vectorAccessor .~ ((p1 : v1) : vs)
    _ -> state

instructionConjEnd :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionConjEnd primAccessor vectorAccessor state = 
  case (uncons (view primAccessor state), uncons (view vectorAccessor state)) of
    (Just (p1,ps), Just (v1,vs)) -> state & primAccessor .~ ps & vectorAccessor .~ ((v1 <> [p1]) : vs)
    _ -> state

-- v for vector, vs for vectorstack (also applicable to strings)
-- Could abstract this unconsing even further in all functions below
instructionTakeN :: State -> Lens' State [[a]] -> State
instructionTakeN state@(State {_int = i1 : is}) accessor = 
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (take (absNum i1 v1) v1 : vs)
    _ -> state
instructionTakeN state _ = state

instructionSubVector :: State -> Lens' State [[a]] -> State
instructionSubVector state@(State {_int = i1 : i2 : is}) accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (subList i1 i2 v1 : vs)
    _ -> state
instructionSubVector state _ = state

instructionVectorFirst :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorFirst state primAccessor vectorAccessor =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) ->
      case uncons v1 of
        Just (vp1, _) -> state & primAccessor .~ (vp1 : view primAccessor state) & vectorAccessor .~ vs
        _ -> state
    _ -> state

instructionVectorLast :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorLast state primAccessor vectorAccessor =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) ->
      case uncons (drop (length v1 - 1) v1) of -- gonna keep this implementation over using last as this can't error
        Just (vplast, _) -> state & primAccessor .~ (vplast : view primAccessor state) & vectorAccessor .~ vs
        _ -> state
    _ -> state

instructionVectorNth :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorNth state@(State {_int = i1 : is}) primAccessor vectorAccessor =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) -> state{_int = is} & primAccessor .~ (v1 !! absNum i1 v1 : view primAccessor state{_int = is}) & vectorAccessor .~ vs
    _ -> state
instructionVectorNth state _ _ = state

instructionRest :: State -> Lens' State [[a]] -> State
instructionRest state accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (drop 1 v1 : vs)
    _ -> state

instructionButLast :: State -> Lens' State [[a]] -> State
instructionButLast state accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (safeInit v1 : vs)
    _ -> state

instructionLength :: State -> Lens' State [[a]] -> State
instructionLength state@(State {_int = is}) accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = length v1 : is} & accessor .~ vs
    _ -> state

instructionReverse :: State -> Lens' State [[a]] -> State
instructionReverse state accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (reverse v1 : vs)
    _ -> state

instructionPushAll :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionPushAll state primAccessor vectorAccessor =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) -> state & vectorAccessor .~ vs & primAccessor .~ (v1 <> view primAccessor state)
    _ -> state

instructionVectorMakeEmpty :: State -> Lens' State [[a]] -> State
instructionVectorMakeEmpty state accessor = state & accessor .~ ([] : view accessor state)

instructionVectorIsEmpty :: State -> Lens' State [[a]] -> State
instructionVectorIsEmpty state@(State {_bool = bs}) accessor =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_bool = null v1 : bs} & accessor .~ vs
    _ -> state

instructionVectorContains :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorContains state@(State {_bool = bs}) primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> state{_bool = (findSubA v1 [p1] /= -1) : bs} & vectorAccessor .~ vs & primAccessor .~ ps
    _ -> state

-- I couldn't think of a better way of doing this
instructionVectorIndexOf :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorIndexOf state primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> (state & vectorAccessor .~ vs & primAccessor .~ ps) & int .~ (findSubA v1 [p1] : view int (state & vectorAccessor .~ vs & primAccessor .~ ps))
    _ -> state

instructionVectorOccurrencesOf :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorOccurrencesOf state primAccessor vectorAccessor = 
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> (state & vectorAccessor .~ vs & primAccessor .~ ps) & int .~ (amtOccurences v1 [p1] : view int (state & vectorAccessor .~ vs & primAccessor .~ ps))
    _ -> state

-- | This function parses the primitives of a vector type and pushes split up onto their
-- respective stack
instructionVectorParseToPrim :: Lens' State [[a]] -> State -> State
instructionVectorParseToPrim accessor state =
  case uncons (view accessor state) of
    Just (x1, xs) -> state & accessor .~ (chunksOf 1 x1 <> xs)
    _ -> state

instructionVectorSetNth :: State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorSetNth state@(State {_int = i1 : is}) primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state{_int = is}), uncons (view primAccessor state{_int = is})) of
    (Just (v1, vs), Just (p1, ps)) -> state{_int = is} & vectorAccessor .~ (replaceAt (absNum i1 v1) p1 v1 : vs) & primAccessor .~ ps
    _ -> state
instructionVectorSetNth state _ _ = state
    
instructionVectorReplace :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorReplace state primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, p2 : ps)) -> state & vectorAccessor .~ (replace v1 [p1] [p2] Nothing : vs) & primAccessor .~ ps
    _ -> state

instructionVectorReplaceFirst :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorReplaceFirst state primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, p2 : ps)) -> state & vectorAccessor .~ (replace v1 [p1] [p2] (Just 1) : vs) & primAccessor .~ ps
    _ -> state

instructionVectorRemove :: Eq a => State -> Lens' State [a] -> Lens' State [[a]] -> State
instructionVectorRemove state primAccessor vectorAccessor =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> state & vectorAccessor .~ (replace v1 [p1] [] Nothing : vs) & primAccessor .~ ps
    _ -> state
  
instructionVectorIterate :: State -> Lens' State [a] -> Lens' State [[a]] -> ([a] -> Gene) -> (State -> State) -> String -> State
instructionVectorIterate state@(State {_exec = e1 : es}) primAccessor vectorAccessor vectorType typeIterateFunction typeIterateFunctionName =
  case uncons (view vectorAccessor state) of
    Just ([], vs) -> state{_exec = es} & vectorAccessor .~ vs
    Just ([x], vs) -> state & primAccessor .~ (x : view primAccessor state) & vectorAccessor .~ vs
    Just (v1, vs) ->
      (case uncons v1 of
        Just (nv1, nvs) -> state{_exec = e1 : vectorType nvs : StateFunc (typeIterateFunction, typeIterateFunctionName) : e1 : es} & primAccessor .~ (nv1 : view primAccessor state) & vectorAccessor .~ vs 
        _ -> state) -- This should never happen
    _ -> state
instructionVectorIterate state _ _ _ _ _ = state

instructionCodeFrom :: State -> Lens' State [a] -> (a -> Gene) -> State
instructionCodeFrom state@(State {_code = cs}) accessor geneType =
  case uncons (view accessor state) of
    Just (x, xs) -> state{_code = geneType x : cs} & accessor .~ xs
    _ -> state

-- |A function that sorts the first vector for a vectorType
instructionVectorSort :: Ord a => Lens' State [[a]] -> State -> State
instructionVectorSort accessor state =
  case uncons (view accessor state) of
    Just (x, xs) -> state & accessor .~ (sort x : xs)
    _ -> state

-- |A function that sorts the first vector in reverse order for a vectorType
instructionVectorSortReverse :: Ord a => Lens' State [[a]] -> State -> State
instructionVectorSortReverse accessor state =
  case uncons (view accessor state) of
    Just (x, xs) -> state & accessor .~ (sortBy (comparing Data.Ord.Down) x : xs)
    _ -> state
