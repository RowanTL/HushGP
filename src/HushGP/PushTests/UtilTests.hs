module HushGP.PushTests.UtilTests where

import HushGP.Instructions.Utility
import Test.QuickCheck

prop_DeleteAtTest :: Int -> [Int] -> Property
prop_DeleteAtTest idx lst =
  idx >= 0 && idx < length lst ==>
  if null lst
  then length lst === length (deleteAt idx lst)
  else length lst === length (deleteAt idx lst) + 1

prop_CombineTupleTest :: Int -> ([Int], [Int]) -> Property
prop_CombineTupleTest val tup =
  length (fst tup) + length (snd tup) === length (combineTuple val tup) - 1

prop_CombineTupleListTest :: [Int] -> ([Int], [Int]) -> Property
prop_CombineTupleListTest lst tup =
  length (fst tup) + length (snd tup) === length (combineTupleList lst tup) - length lst

-- Could use forAll to only generate valid tests
prop_InsertAt :: Int -> Int -> [Int] -> Property
prop_InsertAt idx val lst =
  idx >= 0 && idx < length lst ==>
  length lst === length (insertAt idx val lst) - 1 .&&.
  insertAt idx val lst !! idx === val

prop_ReplaceAt :: Int -> Int -> [Int] -> Property
prop_ReplaceAt idx val lst =
  idx >= 0 && idx < length lst ==>
  length lst === length (replaceAt idx val lst) .&&.
  replaceAt idx val lst !! idx === val

-- prop_SubList :: Int -> Int -> [Int] -> Property
-- prop_SubList idx0 idx1 lst =
  -- idx
