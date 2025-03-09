module HushGP.PushTests.GenericTests where

-- import HushGP.State
-- import Control.Lens
-- import Test.Tasty.QuickCheck
-- -- import HushGP.Instructions.GenericInstructions

-- -- The naming scheme:
-- -- the letters at the beginning represent what kind of transformation (the word I'm using for a basic function) to the states is happening
-- --   for example: the function aaa1Test relays this arg takes a transformation of two as and turns them into one a
-- -- the numbers represent how many different stacks are used in the function.
-- --   for example: the aaa1Test relays that it takes one stack as input. These stacks are passed in as Lens

-- -- We may be able to get rid of Lens entirely and use haskell's integrated accessors of type State -> [a]
-- -- You can see what I'm talking about if you go into ghci and type: `:info _int` for example

-- | Test to see if the length difference between the two stacks post execution is off by one.
-- Based on a primitive lens. Should only be used with functions that modify the length of one stack
-- by one. The first Int specifies what size the stacks should differ by. The second Int
-- specifies how many intial items should be in the stack to not be considered a no-op.
-- diff1Test :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> Int -> State -> Property
-- diff1Test accessor instruction ltAmt state
--   | length (view accessor state) < ltAmt = state === instruction state
--   | otherwise = state =/= instruction state

-- -- aa1Test :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> (a -> a) -> State -> Property
-- -- aa1Test accessor instruction transformation state =
-- --   case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
-- --     (Just (origx1, _), Just (modx1, _)) -> transformation origx1 === modx1 .&&. length (view accessor state) === length (view accessor $ instruction state)
-- --     _ -> state === instruction state

-- -- | Test to see if the length difference between 2 separate stacks post execution if
-- -- the up/down by a passed amt for the respective stats. Is used to test functions like instructionIntFromFloat.
-- diff2Test :: (Show b, Eq b) => Lens' State [a] -> Lens' State [b] -> (State -> State) -> Int -> State -> Property
-- diff2Test accessorFrom accessorTo instruction ltAmt state
--   | length (view accessorFrom state) < ltAmt = state === instruction state
--   | otherwise = length (view accessorTo $ instruction state) =/= length (view accessorTo state) .&&.
--       length (view accessorFrom $ instruction state) =/= length (view accessorFrom state)
  -- case (uncons (view accessorTo $ instruction state), uncons (view accessorFrom state)) of
    -- (Just (_, _), Just (_, _)) -> 
    --   length (view accessorTo $ instruction state) === length (view accessorTo state) + 1 .&&.
    --   length (view accessorFrom $ instruction state) === length (view accessorFrom state) - 1 
    -- _ -> state === instruction state

-- aab2Test :: (Show b, Eq b) => Lens' State [a] -> Lens' State [b] -> (State -> State) -> State -> Property
-- aab2Test accessorFrom accessorTo instruction state =
--   case (uncons (view accessorTo $ instruction state), uncons (view accessorFrom state)) of
--     (Just (_, _), Just (_, _ : _)) ->
--       length (view accessorTo $ instruction state) == length (view accessorTo state) + 1 .&&.
--       length (view accessorFrom $ instruction state) == length (view accessorFrom state) - 2       
--     _ -> state === instruction state

-- popTest :: (Show a) => Lens' State [a] -> (State -> State) -> State -> Property
-- popTest accessor instruction state = 
--   if null $ view accessor state 
--   then state === instruction state 
--   else length (view accessor $ instruction state) === length (view accessor state) - 1

-- dupTest :: (Eq a, Show a) => Lens' State [a] -> (State -> State) -> State -> Property
-- dupTest accessor instruction state =
--   case uncons (view accessor state) of
--     Just (origx1, _) ->
--       case uncons (view accessor $ instruction state) of
--         Just (modx1, modx2 : _) ->
--           origx1 === modx1 .&&. origx1 === modx2 .&&. length (view accessor $ instruction state) === length (view accessor state) + 1
--         _ -> state === instruction state
--     _ -> state === instruction state
    
-- -- How to test the int stack in particular?
-- dupTestN :: (Eq a, Show a) => Lens' State [a] -> (State -> State) -> State -> Property
-- dupTestN accessor instruction state =
--   case uncons (view int state) of
--     Just (i1, is) ->
--       let amt = max i1 0 in
--       case uncons (view accessor state{_int = is}) of
--         Just (origx1, _) ->
--           conjoin (map (origx1 ===) (take amt (view accessor $ instruction state))) .&&.
--           length (view accessor $ instruction state) === (length (view accessor state{_int = is}) + amt - 1)
--         _ -> state === instruction state
--     _ -> state === instruction state

-- swapTest :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
-- swapTest accessor instruction state =
--   case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
--     (Just (origx1, origx2 : _), Just (modx1, modx2 : _)) -> origx1 === modx2 .&&. origx2 === modx1
--     _ -> state === instruction state

-- rotTest :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
-- rotTest accessor instruction state =
--   case (uncons (view accessor state), uncons (view accessor $ instruction state)) of
--     (Just (origx1, origx2 : origx3 : _), Just (modx1, modx2 : modx3 : _)) -> (origx1, origx2, origx3) === (modx2, modx3, modx1)
--     _ -> state === instruction state

-- flushTest :: (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
-- flushTest accessor instruction state =
--   property $ null $ view accessor $ instruction state

-- stackDepthTest :: (Show a) => Lens' State [a] -> (State -> State) -> State -> Property
-- stackDepthTest accessor instruction state =
--   case uncons (view int $ instruction state) of
--     Just (x1, _) -> x1 === length (view accessor state)
--     _ -> state === instruction state

-- yankTest :: forall a. (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
-- yankTest accessor instruction state@(State {_int = i1 : is}) =
--   let
--     myIndex :: Int
--     myIndex = max 0 (min i1 (length (view accessor state{_int = is}) - 1))
--     item :: a
--     item = view accessor state{_int = is} !! myIndex
--   in
--   case (uncons (view accessor $ instruction state), uncons is) of
--     (Just (x1, _), Just (_, _)) -> x1 === item
--     _ -> state === instruction state
--   -- .&&.  -- unsure how to get this functional
--   -- length (view accessor state{_int = is}) === length (view accessor $ instruction state)
-- yankTest _ instruction state = state === instruction state

-- -- Might just make this a unit test
-- -- Come back to this later
-- -- yankDupTest :: forall a. (Show a, Eq a) => Lens' State [a] -> (State -> State) -> State -> Property
-- -- yankDupTest accessor instruction state@(State {_int = i1 : is}) =
-- --   let
-- --     myIndex :: Int
-- --     myIndex = max 0 (min i1 (length (view accessor state{_int = is}) - 1))
-- --     item :: a
-- --     item = view accessor state{_int = is} !! myIndex
-- --   in
-- --   case (uncons (view accessor $ instruction state), uncons is) of
-- --     (Just (x1, xs), Just (_, _)) -> x1 === item .&&. (x1 : xs) !!  === item
-- --     _ -> state === instruction state
-- -- yankDupTest _ instruction state = state === instruction state

-- -- shoveTest
