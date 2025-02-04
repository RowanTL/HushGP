module PushTests.GenericTests where

import State
import Control.Lens
import Debug.Trace
import Test.QuickCheck

-- The naming scheme:
-- the letters at the beginning represent what kind of transformation (the word I'm using for a basic function) to the states is happening
--   for example: the function aaa1Test relays this arg takes a transformation of two as and turns them into one a
-- the numbers represent how many different stacks are used in the function.
--   for example: the aaa1Test relays that it takes one stack as input. These stacks are passed in as Lens

-- We may be able to get rid of Lens entirely and use haskell's integrated accessors of type State -> [a]
-- You can see what I'm talking about if you go into ghci and type: `:info _int` for example

aaa1Test :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> (a -> a -> a) -> State -> Property
aaa1Test accessor instruction transformation state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, origx2 : _), Just (modx1, _)) -> transformation origx2 origx1 === modx1 .&&. length (view accessor state) === length (view accessor $ instruction state) + 1
    _ -> state === instruction state

aa1Test :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> (a -> a) -> State -> Property
aa1Test accessor instruction transformation state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, _), Just (modx1, _)) -> transformation origx1 === modx1 .&&. length (view accessor state) === length (view accessor $ instruction state)
    _ -> state === instruction state

ab1Test :: (Show b, Eq b) => Lens' State [a] -> Lens' State [b] -> (State -> State) -> (a -> b) -> State -> Property
ab1Test accessorFrom accessorTo instruction transformation state =
  case (uncons (view accessorTo $ instruction state), uncons (view accessorFrom state)) of
    (Just (t1, _), Just (f1, _)) -> 
      t1 === transformation f1 .&&. 
      length (view accessorTo $ instruction state) === length (view accessorTo state) + 1 .&&.
      length (view accessorFrom $ instruction state) === length (view accessorFrom state) - 1 
    _ -> state === instruction state

aab2Test :: (Show b, Eq b) => Lens' State [a] -> Lens' State [b] -> (State -> State) -> (a -> a -> b) -> State -> Property
aab2Test accessorFrom accessorTo instruction transformation state =
  case (uncons (view accessorTo $ instruction state), uncons (view accessorFrom state)) of
    (Just (t1, _), Just (f1, f2 : _)) ->
      t1 === transformation f1 f2 .&&. 
      length (view accessorTo $ instruction state) == length (view accessorTo state) + 1 .&&.
      length (view accessorFrom $ instruction state) == length (view accessorFrom state) - 2       
    _ -> state === instruction state

popTest :: (Show a) => Lens' State [a] -> (State -> State) -> State -> Property
popTest accessor instruction state = 
  if null $ view accessor state 
  then state === instruction state 
  else length (view accessor $ instruction state) === length (view accessor state) - 1

dupTest :: (Eq a, Show a) => Lens' State [a] -> (State -> State) -> State -> Property
dupTest accessor instruction state =
  case uncons (view accessor state) of
    Just (origx1, _) ->
      case uncons (view accessor $ instruction state) of
        Just (modx1, modx2 : _) ->
          origx1 === modx1 .&&. origx1 === modx2 .&&. length (view accessor $ instruction state) === length (view accessor state) + 1
        _ -> state === instruction state
    _ -> state === instruction state
    
-- How to test the int stack in particular?
dupTestN :: (Eq a, Show a) => Lens' State [a] -> (State -> State) -> State -> Property
dupTestN accessor instruction state =
  case uncons (view int state) of
    Just (i1, is) ->
      let amt = max i1 0 in
      case uncons (view accessor state{_int = is}) of
        Just (origx1, _) ->
          conjoin (map (origx1 ===) (take amt (view accessor $ instruction state))) .&&.
          length (view accessor $ instruction state) === (length (view accessor state{_int = is}) + amt - 1)
        _ -> state === instruction state
    _ -> state === instruction state

swapTest :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
swapTest accessor instruction state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, origx2 : _), Just (modx1, modx2 : _)) -> origx1 === modx2 .&&. origx2 === modx1
    _ -> state === instruction state

rotTest :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
rotTest accessor instruction state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, origx2 : origx3 : _), Just (modx1, modx2 : modx3 : _)) -> (origx1, origx2, origx3) === (modx2, modx3, modx1)
    _ -> state === instruction state
