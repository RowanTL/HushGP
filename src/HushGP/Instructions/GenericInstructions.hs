module HushGP.Instructions.GenericInstructions where

import Control.Lens
import Data.List (sort, sortBy)
import Data.Ord
import Data.List.Split
import HushGP.State
import HushGP.Instructions.Utility-- import Debug.Trace 

-- |Does No Operation. Useful for genome stuff :)
instructionNoOpBlock :: State -> State
instructionNoOpBlock state = state

-- |Does No Operation. Just evolve fodder.
instructionNoOp :: State -> State
instructionNoOp state = state

-- |Duplicates the top of a stack based on a lens.
instructionDup :: Lens' State [a] -> State  -> State
instructionDup accessor state =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x1,_) -> state & accessor .~ x1 : view accessor state

-- |Pops the top of the stack based on a lens.
instructionPop :: Lens' State [a] -> State -> State
instructionPop accessor state = state & accessor .~ drop 1 (view accessor state)

-- |Pushes True if the lens' stack is empty, False if not.
instructionIsStackEmpty :: Lens' State [a] -> State -> State
instructionIsStackEmpty accessor state@(State {_bool = bs}) = state{_bool = null (view accessor state) : bs}

-- |Duplicates the top of a stack based on a lens and the top of the int stack.
instructionDupN :: forall a. Lens' State [a] -> State -> State
instructionDupN accessor state = 
  case uncons (view int state) of
    Just (i1,is) ->
      case uncons (view accessor state{_int = is}) of
        Just (a1,as) -> 
          instructionDupNHelper i1 a1 accessor (state{_int = is} & accessor .~ as)
        _ -> state
    _ -> state
  where
    instructionDupNHelper :: Integral b => b -> a -> Lens' State [a] -> State -> State
    instructionDupNHelper count instruction internalAccessor internalState =
      if count > 0
      then instructionDupNHelper (count - 1) instruction internalAccessor (internalState & accessor .~ (instruction : view accessor internalState))
      else internalState

-- |Duplicates the top N items on a stack. If n <= 0, nothing happens
-- TODO: Will need to implement a max stack items at some point
instructionDupItems :: Lens' State [a] -> State -> State
instructionDupItems accessor state@(State {_int = i1 : is}) =
  if i1 <= 0
  then state{_int = is}
  else state{_int = is} & accessor .~ (take (fromIntegral i1) (view accessor state{_int = is}) <> view accessor state{_int = is})
instructionDupItems _ state = state

-- |Swaps the top two instructions based on a lens
instructionSwap :: Lens' State [a] -> State -> State
instructionSwap accessor state =
  state & accessor .~ swapper (view accessor state)
  where
    swapper :: [a] -> [a]
    swapper (x1 : x2 : xs) = x2 : x1 : xs
    swapper xs = xs

-- |Rotates top 3 integers based on a lens.
-- We could use template haskell to rotate any number of these as
-- an instruction later.
instructionRot :: Lens' State [a] -> State -> State
instructionRot accessor state =
  state & accessor .~ rotator (view accessor state)
  where
    rotator :: [a] -> [a]
    rotator (x1 : x2 : x3 : xs) = x3 : x1 : x2 : xs
    rotator xs = xs

-- |Deletes all instructions in a stack based on a lens.
instructionFlush :: Lens' State [a] -> State -> State
instructionFlush accessor state = state & accessor .~ []

-- |Checks if the two top instructions are equal based on a lens.
-- Pushes the result to the bool stack.
instructionEq :: forall a. Eq a => Lens' State [a] -> State -> State
instructionEq accessor state =
  case uncons $ view accessor state of
    Nothing -> state
    Just (x1, x2 : _) -> droppedState & bool .~ (x1 == x2) : view bool droppedState
    Just _ -> state
  where
    droppedState :: State
    droppedState = state & accessor .~ drop 2 (view accessor state)

-- |Calculates the stack depth based on a lens and pushes the result to the int stackk.
instructionStackDepth :: Lens' State [a] -> State -> State
instructionStackDepth accessor state@(State {_int = is}) = state{_int = toInteger (length (view accessor state)) : is}

-- |Copies an item from deep within a lens' stack to the top of the lens' stack based on
-- the top int from the int stack.
instructionYankDup :: Lens' State [a] -> State -> State
instructionYankDup accessor state@(State {_int = i1 : is}) = 
  if notEmptyStack accessor state{_int = is}
  then state{_int = is} & accessor .~ (view accessor state{_int = is} !! max 0 (min (fromIntegral i1) (length (view accessor state{_int = is}) - 1))) : view accessor state{_int = is}
  else state
instructionYankDup  _ state = state

-- |Moves an item from deep within a lens' stack to the top of the lens' stack based on
-- the top int from the int stack.
instructionYank :: forall a. Lens' State [a] -> State -> State
instructionYank accessor state@(State {_int = i1 : is}) =
  let
    myIndex :: Int
    myIndex = max 0 (min (fromIntegral i1) (length (view accessor state{_int = is}) - 1))
    item :: a
    item = view accessor state{_int = is} !! myIndex
    deletedState :: State
    deletedState = state{_int = is} & accessor .~ deleteAt myIndex (view accessor state{_int = is})
  in
  if notEmptyStack accessor state{_int = is} then deletedState & accessor .~ item : view accessor deletedState else state
instructionYank _ state = state

-- |Copies an item from the top of a lens' stack to deep within the lens' stack based on
-- the top int from the int stack.
-- In pysh, instructionShoveDup and instructionShove behave differently when indexing in such a way that
-- the duplicated index matters whether or not it's present in the stack at the moment of calculation.
-- I'm not going to keep this behavior. Check out interpysh examples for how pysh handles it.
instructionShoveDup :: Lens' State [a] -> State -> State
instructionShoveDup accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state{_int = is}) of
    Just (x,_) -> state{_int = is} & accessor .~ combineTuple x (splitAt (max 0 (min (fromIntegral i1) (length (view accessor state{_int = is}) - 1))) (view accessor state{_int = is}))
    _ -> state
instructionShoveDup _ state = state

-- |Moves an item from the top of a lens' stack to deep within the lens' stack based on
-- the top int from the int stack.
instructionShove :: Lens' State [a] -> State -> State
instructionShove accessor state = state & accessor .~ drop 1 (view accessor (instructionShoveDup accessor state ))

-- |Concats two semigroupable items together based on a lens. Not char generic.
instructionVectorConcat :: Semigroup a => Lens' State [a] -> State -> State
instructionVectorConcat accessor state =
  case uncons (view accessor state) of
    Just (x1, x2:_) -> droppedState & accessor .~ (x1 <> x2) : view accessor droppedState
    _ -> state
  where
    droppedState :: State
    droppedState = state & accessor .~ drop 2 (view accessor state)

-- |Based on two lenses, one of a primitive type and the next of a vector type, 
-- takes the top item of the primitive stack and prepends it to the first vector in
-- the vector stack if there is one.
instructionVectorConj :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorConj primAccessor vectorAccessor state =
  case (uncons (view primAccessor state), uncons (view vectorAccessor state)) of
    (Just (p1,ps), Just (v1,vs)) -> state & primAccessor .~ ps & vectorAccessor .~ ((p1 : v1) : vs)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type, 
-- takes the top item of the primitive stack and appends it to the first vector in
-- the vector stack if there is one.
instructionVectorConjEnd :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorConjEnd primAccessor vectorAccessor state = 
  case (uncons (view primAccessor state), uncons (view vectorAccessor state)) of
    (Just (p1,ps), Just (v1,vs)) -> state & primAccessor .~ ps & vectorAccessor .~ ((v1 <> [p1]) : vs)
    _ -> state

-- |Takes the first N items from the first vector on the top of a vector stack and
-- pushes the result to said vector stack.
instructionVectorTakeN :: Lens' State [[a]] -> State -> State
instructionVectorTakeN accessor state@(State {_int = i1 : is}) = 
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (take (absNum i1 v1) v1 : vs)
    _ -> state
instructionVectorTakeN _ state = state

-- |Takes the last N items from the first vector on the top of a vector stack and
-- pushes the result to said vector stack.
instructionVectorTakeRN :: Lens' State [[a]] -> State -> State
instructionVectorTakeRN accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (takeR (absNum i1 v1) v1 : vs)
    _ -> state
instructionVectorTakeRN _ state = state

-- |Takes the sublist of the top vector based on a passed lens. Check out the
-- subList documentation for information on how this works.
instructionSubVector :: Lens' State [[a]] -> State -> State
instructionSubVector accessor state@(State {_int = i1 : i2 : is}) =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (subList (fromIntegral i1) (fromIntegral i2) v1 : vs)
    _ -> state
instructionSubVector _ state = state

-- |Based on two lenses, one of a primitive type and the next of a vector type, 
-- Takes the first item from the top vector and places it onto the passed primitive stack.
instructionVectorFirst :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorFirst primAccessor vectorAccessor state =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) ->
      case uncons v1 of
        Just (vp1, _) -> state & primAccessor .~ (vp1 : view primAccessor state) & vectorAccessor .~ vs
        _ -> state
    _ -> state

-- |Based on a vector lens, takes the first item from the top vector on the vector stack
-- and creates a vector wrapping that first item, pushing it back onto the stack.
-- Not to be confused with instructionVectorFromFirstPrim.
instructionVectorFromFirstPrim :: Lens' State [[a]] -> State -> State
instructionVectorFromFirstPrim accessor state =
  case uncons (view accessor state) of
    Just (v1, vs) ->
      case uncons v1 of
        Just (vp1, _) -> state & accessor .~ ([vp1] : vs)
        _ -> state
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type, 
-- pushes the top item of the primitive stack wrapped in a list to the top of the
-- vector stack. Not to be confused with instructionVectorFromFirstPrim.
instructionVectorFromPrim :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorFromPrim primAccessor vectorAccessor state =
  case uncons (view primAccessor state) of
    Just (p1, ps) -> state & primAccessor .~ ps & vectorAccessor .~ ([p1] : view vectorAccessor state)
    _ -> state
    

-- |Based on two lenses, one of a primitive type and the next of a vector type, 
-- Takes the last item from the top vector and places it onto the passed primitive stack.
instructionVectorLast :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorLast primAccessor vectorAccessor state =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) ->
      case uncons (drop (length v1 - 1) v1) of -- gonna keep this implementation over using last as this can't error
        Just (vplast, _) -> state & primAccessor .~ (vplast : view primAccessor state) & vectorAccessor .~ vs
        _ -> state
    _ -> state

-- |Based on a vector lens, takes the last item from the top vector on the vector stack
-- and creates a vector wrapping that last item, pushing it back onto the stack.
instructionVectorFromLastPrim :: Lens' State [[a]] -> State -> State
instructionVectorFromLastPrim accessor state =
  case uncons (view accessor state) of
    Just (v1, vs) ->
      case uncons (drop (length v1 - 1) v1) of
        Just (vp1, _) -> state & accessor .~ ([vp1] : vs)
        _ -> state
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- Takes the Nth item from the top vector and places it onto the passed primitive stack
-- based on an int from the int stack.
instructionVectorNth :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorNth primAccessor vectorAccessor state@(State {_int = i1 : is}) =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) -> state{_int = is} & primAccessor .~ (v1 !! absNum i1 v1 : view primAccessor state{_int = is}) & vectorAccessor .~ vs
    _ -> state
instructionVectorNth _ _ state= state

-- |Based on a vector lens, takes the Nth item from the top vector on the vector stack
-- and creates a vector wrapping that Nth item, pushing it back onto the stack. N is
-- the top item on the int stack.
instructionVectorFromNthPrim :: Lens' State [[a]] -> State -> State
instructionVectorFromNthPrim accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = is} & accessor .~ ([v1 !! absNum i1 v1] : vs)
    _ -> state
instructionVectorFromNthPrim _ state = state

-- |Takes the top vector, removes the first item of said vector, and pushes the result back to top
-- of the stack, based on a lens.
instructionVectorRest :: Lens' State [[a]] -> State -> State
instructionVectorRest accessor state =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (drop 1 v1 : vs)
    _ -> state

-- |Takes the top vector, removes the last item of said vector, and pushes the result back to top
-- of the stack, based on a vector lens.
instructionVectorButLast :: Lens' State [[a]] -> State -> State
instructionVectorButLast accessor state =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (safeInit v1 : vs)
    _ -> state

-- |Based on a vector lens, drops the first N items from the top vector.
-- Pushes the result back to the vector stack. N is pulled from the top
-- of the int stack.
instructionVectorDrop :: Lens' State [[a]] -> State -> State
instructionVectorDrop accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state{_int = is}) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (drop (absNum i1 v1) v1 : vs)
    _ -> state
instructionVectorDrop _ state = state

-- |Based on a vector lens, drops the last N items from the top vector.
-- Pushes the result back to the vector stack. N is pulled from the top
-- of the int stack.
instructionVectorDropR :: Lens' State [[a]] -> State -> State
instructionVectorDropR accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state{_int = is}) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (dropR (absNum i1 v1) v1 : vs)
    _ -> state
instructionVectorDropR _ state = state

-- |Takes the top vector, pushes the length of that vector to the int stack, based on a vector lens.
instructionLength :: Lens' State [[a]] -> State -> State
instructionLength accessor state@(State {_int = is}) =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_int = toInteger (length v1) : is} & accessor .~ vs
    _ -> state

-- |Takes the top vector, reverses it, based on a lens.
instructionReverse :: Lens' State [[a]] -> State -> State
instructionReverse accessor state =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ (reverse v1 : vs)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- takes the vector and individually pushes its indices to the passed primitive stack.
instructionPushAll :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionPushAll primAccessor vectorAccessor state =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) -> state & vectorAccessor .~ vs & primAccessor .~ (v1 <> view primAccessor state)
    _ -> state

-- |Based on a vector lens, makes an empty vector and pushes it to the passed stack.
instructionVectorMakeEmpty :: Lens' State [[a]] -> State -> State
instructionVectorMakeEmpty accessor state = state & accessor .~ ([] : view accessor state)

-- |Based on a vector lens, checks if the top vector is empty. If so, pushes True to the
-- bool stack. If not, pushes False.
instructionVectorIsEmpty :: Lens' State [[a]] -> State -> State
instructionVectorIsEmpty accessor state@(State {_bool = bs}) =
  case uncons (view accessor state) of
    Just (v1, vs) -> state{_bool = null v1 : bs} & accessor .~ vs
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- If the vector on the top of the vector stack contains the top item on the primitive stack,
-- pushes True to the bool stack. Pushes False otherwise.
instructionVectorContains :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorContains primAccessor vectorAccessor state@(State {_bool = bs}) =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> state{_bool = (findSubA v1 [p1] /= -1) : bs} & vectorAccessor .~ vs & primAccessor .~ ps
    _ -> state

-- |Based on a vector lens and the two vectors on the top of said stack.
-- If the second vector can be found within the first vector, True is pushed to the
-- bool stack. If not, False is pushed to the bool stack.
instructionVectorContainsVector :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorContainsVector accessor state@(State {_bool = bs}) =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) -> state & accessor .~ vs & bool .~ ((findSubA v1 v2 /= (-1)) : bs)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- finds the first index of the top item in the primitive stack inside of the
-- top vector from the vector stack and pushes the result to the int stack.
instructionVectorIndexOf :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorIndexOf primAccessor vectorAccessor state =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> (state & vectorAccessor .~ vs & primAccessor .~ ps) & int .~ (toInteger (findSubA v1 [p1]) : view int (state & vectorAccessor .~ vs & primAccessor .~ ps))
    _ -> state

-- |Based on a vector lens and the two vectors on top of said stack. Searches and pushes the
-- index of the second vector inside of the first vector to the int stack. Pushes -1 if not found.
instructionVectorIndexOfVector :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorIndexOfVector accessor state@(State {_int = is}) =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) -> state & accessor .~ vs & int .~ (toInteger (findSubA v1 v2) : is)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- finds the amount of times the top item in the primitive stack occurs inside of the
-- top vector from the vector stack and pushes the result to the int stack.
instructionVectorOccurrencesOf :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorOccurrencesOf primAccessor vectorAccessor state = 
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> (state & vectorAccessor .~ vs & primAccessor .~ ps) & int .~ (toInteger (amtOccurences v1 [p1]) : view int (state & vectorAccessor .~ vs & primAccessor .~ ps))
    _ -> state

-- |Based on a vector lens and the top two vectors in said stack,
-- Counts the amount of occurrences of the second vector in the first
-- vector. Pushes the result to the string stack.
instructionVectorOccurrencesOfVector :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorOccurrencesOfVector accessor state@(State {_int = is}) =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) -> state & accessor .~ vs & int .~ (toInteger (amtOccurences v1 v2) : is)
    _ -> state

-- |This function parses the primitives inside a vector type and pushes that vector split into
-- lists of size one and pushes the result onto the respective vector stack. Based on a vector lens.
instructionVectorParseToPrim :: Lens' State [[a]] -> State -> State
instructionVectorParseToPrim accessor state =
  case uncons (view accessor state) of
    Just (x1, xs) -> state & accessor .~ (chunksOf 1 x1 <> xs)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type.
-- Sets the Nth index inside of the top vector from the vector stack to the top value
-- from the primitive stack. N is based on an int from the top of the int stack.
instructionVectorSetNth :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorSetNth primAccessor vectorAccessor state@(State {_int = i1 : is}) =
  case (uncons (view vectorAccessor state{_int = is}), uncons (view primAccessor state{_int = is})) of
    (Just (v1, vs), Just (p1, ps)) -> state{_int = is} & vectorAccessor .~ (replaceAt (absNum i1 v1) p1 v1 : vs) & primAccessor .~ ps
    _ -> state
instructionVectorSetNth _ _ state = state

-- |Based on two lenses, one of a primitive type and the next of a vector type.
-- Splits the vector on top of the vector stack with the top primitive and pushes the
-- result to the original vector stack.
instructionVectorSplitOn :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorSplitOn primAccessor vectorAccessor state =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> state & primAccessor .~ ps & vectorAccessor .~ (reverse (splitOn [p1] v1) <> vs)
    _ -> state

-- |Based on a vector lens and top two items of said stack, splits the
-- first vector based on the second vector and pushes the result to the
-- original vector stack.
instructionVectorSplitOnVector :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorSplitOnVector accessor state =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) -> state & accessor .~ (reverse (splitOn v2 v1) <> vs)
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- replaces Maybe Int occurrences inside of the top vector from the vector stack with two values from
-- the primitive stack. The top of the primitive stack is the old value to be replaced. The second item
-- in the primitive stack is the new value to replace the old one. Nothing replaces all occurrences.
instructionVectorReplace :: Eq a => Lens' State [a] -> Lens' State [[a]] -> Maybe Int -> State -> State
instructionVectorReplace primAccessor vectorAccessor amt state =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, p2 : ps)) -> state & vectorAccessor .~ (replace v1 [p1] [p2] amt: vs) & primAccessor .~ ps
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- replaces N occurrences inside of the top vector from the vector stack with two values from
-- the primitive stack. The top of the primitive stack is the old value to be replaced. The second item
-- in the primitive stack is the new value to replace the old one. N is pulled from the top of the int stack.
instructionVectorReplaceN :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorReplaceN primAccessor vectorAccessor state@(State {_int = i1 : is}) = instructionVectorReplace primAccessor vectorAccessor (Just (fromIntegral i1)) state{_int = is}
instructionVectorReplaceN _ _ state = state

-- |Based on a vector lens and the top three vectors on said stack.
-- Inside of the first vector, replaces the number of instances specified
-- by the Maybe Int parameter of the second vector with the third vector.
-- If amt is Nothing, replaces all instances.
instructionVectorReplaceVector :: Eq a => Lens' State [[a]] -> Maybe Int -> State -> State
instructionVectorReplaceVector accessor amt state =
  case uncons (view accessor state) of
    Just (v1, v2 : v3 : vs) -> state & accessor .~ (replace v1 v2 v3 amt : vs)
    _ -> state

-- |Based on a vector lens, the top three vectors on said stack, and the top int on the int stack.
-- Inside of the first vector, replaces the number of instances specified
-- by the top of the int stack of the second vector with the third vector.
instructionVectorReplaceVectorN :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorReplaceVectorN accessor state@(State {_int = i1 : is}) = instructionVectorReplaceVector accessor (Just (fromIntegral i1)) state{_int = is}
instructionVectorReplaceVectorN _ state = state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- Removes Maybe Int occurrences inside of the top vector from the vector stack where the top
-- item from the primitive stack equals a primitive inside of the vector stack. If Nothing is passed
-- rather than a Just Int, will remove all occurrences.
instructionVectorRemove :: Eq a => Lens' State [a] -> Lens' State [[a]] -> Maybe Int -> State -> State
instructionVectorRemove primAccessor vectorAccessor amt state =
  case (uncons (view vectorAccessor state), uncons (view primAccessor state)) of
    (Just (v1, vs), Just (p1, ps)) -> state & vectorAccessor .~ (replace v1 [p1] [] amt: vs) & primAccessor .~ ps
    _ -> state

-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- Removes N occurrences inside of the top vector from the vector stack where the top
-- item from the primitive stack equals a primitive inside of the vector stack. N is pulled
-- from the top of the int stack. Not to be confused with instructionVectorRemoveNth.
instructionVectorRemoveN :: Eq a => Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorRemoveN primAccessor vectorAccessor state@(State {_int = i1 : is}) = instructionVectorRemove primAccessor vectorAccessor (Just (fromIntegral i1)) state{_int = is}
instructionVectorRemoveN _ _ state = state

-- |Based on a vector lens. Removes the Nth index of the top vector of the passed
-- vector stack. N is pulled from the top of the int stack. Not to be confused with
-- instructionVectorRemoveN.
instructionVectorRemoveNth :: Lens' State [[a]] -> State -> State
instructionVectorRemoveNth accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state{_int = is}) of
    Just (v1, vs) -> state{_int = is} & accessor .~ (deleteAt (absNum i1 v1) v1 : vs)
    _ -> state
instructionVectorRemoveNth _ state = state

-- |Based on a vector lens and the two vectors on top of said stack.
-- Inside of the first vector, removes the number of instances specified
-- by the Maybe Int parameter of the second vector. Nothing removes all instances.
instructionVectorRemoveVector :: Eq a => Lens' State [[a]] -> Maybe Int -> State -> State
instructionVectorRemoveVector accessor amt state =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) -> state & accessor .~ (replace v1 v2 [] amt : vs)
    _ -> state

-- |Based on a vector lens, the top two vectors on said stack, and the top int on the int stack.
-- Inside of the first vector, removes the number of instances specified
-- by the top of the int stack of the second vector.
instructionVectorRemoveVectorN :: Eq a => Lens' State [[a]] -> State -> State
instructionVectorRemoveVectorN accessor state@(State {_int = i1 : is}) = instructionVectorRemoveVector accessor (Just (fromIntegral i1)) state{_int = is}
instructionVectorRemoveVectorN _ state = state
  
-- |Based on two lenses, one of a primitive type and the next of a vector type,
-- iterates over the top vector from the vector stack using the top code from the code stack.
-- Pysh explains this better.
instructionVectorIterate :: Lens' State [a] -> Lens' State [[a]] -> ([a] -> Gene) -> (State -> State) -> String -> State -> State
instructionVectorIterate primAccessor vectorAccessor vectorType typeIterateFunction typeIterateFunctionName state@(State {_exec = e1 : es}) =
  case uncons (view vectorAccessor state) of
    Just ([], vs) -> state{_exec = es} & vectorAccessor .~ vs
    Just ([x], vs) -> state & primAccessor .~ (x : view primAccessor state) & vectorAccessor .~ vs
    Just (v1, vs) ->
      (case uncons v1 of
        Just (nv1, nvs) -> state{_exec = e1 : vectorType nvs : StateFunc (typeIterateFunction, typeIterateFunctionName) : e1 : es} & primAccessor .~ (nv1 : view primAccessor state) & vectorAccessor .~ vs 
        _ -> state) -- This should never happen
    _ -> state
instructionVectorIterate _ _ _ _ _ state = state

-- |Moves a type from a stack and places it onto the code stack. Based on a primitive stack.
-- The (a -> Gene) is something like GeneBool or GeneInt for example.
instructionCodeFrom :: Lens' State [a] -> (a -> Gene) -> State -> State
instructionCodeFrom accessor geneType state@(State {_code = cs}) =
  case uncons (view accessor state) of
    Just (x, xs) -> state{_code = geneType x : cs} & accessor .~ xs
    _ -> state

-- |Sorts the top vector in a vector stack, based on a vector lens.
instructionVectorSort :: Ord a => Lens' State [[a]] -> State -> State
instructionVectorSort accessor state =
  case uncons (view accessor state) of
    Just (x, xs) -> state & accessor .~ (sort x : xs)
    _ -> state

-- |Sorts the top vector in a vector stack in reverse order for a vectorType, based on a vector lens.
instructionVectorSortReverse :: Ord a => Lens' State [[a]] -> State -> State
instructionVectorSortReverse accessor state =
  case uncons (view accessor state) of
    Just (x, xs) -> state & accessor .~ (sortBy (comparing Data.Ord.Down) x : xs)
    _ -> state

-- |Takes a vector lens, a primitive lens, and the top of the int stack
-- Inserts the top of the primitive stack into a index specified by the
-- top of the int stack into the top vector from the vector stack.
instructionVectorInsert :: Lens' State [a] -> Lens' State [[a]] -> State -> State
instructionVectorInsert primAccessor vectorAccessor state@(State {_int = i1 : is}) =
  case (uncons (view vectorAccessor state{_int = is}), uncons (view primAccessor state{_int = is})) of
    (Just (v1, vs), Just (p1, ps)) -> state{_int = is} & primAccessor .~ ps & vectorAccessor .~ (combineTuple p1 (splitAt (fromIntegral i1) v1) : vs)
    _ -> state
instructionVectorInsert _ _ state = state

-- |Takes a vector lens and inserts the second vector on the vector stack
-- into the first vector on the vector stack based on an int from the
-- int stack.
instructionVectorInsertVector :: Lens' State [[a]] -> State -> State
instructionVectorInsertVector accessor state@(State {_int = i1 : is}) =
  case uncons (view accessor state) of
    Just (v1, v2 : vs) ->
      state{_int = is} & accessor .~ (combineTupleList v2 (splitAt (fromIntegral i1) v1) : vs)
    _ -> state
instructionVectorInsertVector _ state = state

-- |Takes a numeric vector lens and a primitive lens. Pushes the mean of the top
-- vector to the primitive stack.
-- instructionVectorMean :: Fractional a => Lens' State [a] -> Lens' State [[a]] -> (b -> a) -> State -> State
-- instructionVectorMean primAccessor vectorAccessor wrangleFunc state =
--   case uncons (view vectorAccessor state) of
--     Just (v1, vs) -> state & vectorAccessor .~ vs & primAccessor .~ (mean v1 : view primAccessor state)
--     _ -> state

-- |Takes a vector lens, a primitive lens, and an arbitrary function. Pushes the result
-- of applying the arbitrary function to the top vector lens item to the top of the primitive lens stack.
instructionVectorFuncVectorToPrim :: Ord a => Lens' State [a] -> Lens' State [[a]] -> ([a] -> a) -> State -> State
instructionVectorFuncVectorToPrim primAccessor vectorAccessor func state =
  case uncons (view vectorAccessor state) of
    Just (v1, vs) -> state & vectorAccessor .~ vs & primAccessor .~ func v1 : view primAccessor state
    _ -> state

-- |Takes a vector lens and an arbitrary function. Applies the arbitrary function to the top
-- item of the vector lens stack and returns it to said stack.
instructionVectorFuncVectorToVector :: Ord a => Lens' State [[a]] -> ([a] -> [a]) -> State -> State
instructionVectorFuncVectorToVector accessor func state =
  case uncons (view accessor state) of
    Just (v1, vs) -> state & accessor .~ func v1 : vs
    _ -> state
