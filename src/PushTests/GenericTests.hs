module PushTests.GenericTests where

import State
import Control.Lens
import Debug.Trace

arithmeticTest :: (Num a, Eq a) => Lens' State [a] -> (State -> State) -> (a -> a -> a) -> State -> Bool
arithmeticTest accessor instruction func state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, origx2 : _), Just (modx1, _)) -> func origx2 origx1 == modx1 && length (view accessor state) == length (view accessor $ instruction state) + 1
    _ -> state == instruction state

unaryTest :: (Num a, Eq a) => Lens' State [a] -> (State -> State) -> (a -> a) -> State -> Bool
unaryTest accessor instruction func state =
  case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
    (Just (origx1, _), Just (modx1, _)) -> func origx1 == modx1 && length (view accessor state) == length (view accessor $ instruction state)
    _ -> state == instruction state

typeFromType :: Eq b => Lens' State [a] -> Lens' State [b] -> (State -> State) -> (a -> b) -> State -> Bool
typeFromType accessorFrom accessorTo instruction transformation state =
  case (uncons (view accessorTo $ instruction state), uncons (view accessorFrom state)) of
    (Just (t1, _), Just (f1, _)) -> 
      t1 == transformation f1 && 
      length (view accessorTo $ instruction state) == length (view accessorTo state) + 1 &&
      length (view accessorFrom $ instruction state) == length (view accessorFrom state) - 1 
    _ -> state == instruction state
