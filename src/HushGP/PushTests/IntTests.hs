module HushGP.PushTests.IntTests where

import HushGP.State
import HushGP.Instructions.IntInstructions
import HushGP.PushTests.TestStates
import Control.Lens hiding (uncons)
-- import System.Environment
import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO ()
main = do
  -- setEnv "TASTY_QUICKCHECK_MAX_SIZE" "10"
  -- setEnv "TASTY_QUICKCHECK_VERBOSE" "False"
  defaultMain intTests

-- |Holds the tree for property and unit tests.
intTests :: TestTree
intTests = testGroup "All Int Tests" [intUnitTests]

intUnitTests :: TestTree
intUnitTests = testGroup "Unit Tests"
  [ testCase "Int DupN Success" $ view int (instructionIntDupN exampleState) @?= [5, 5, 5, 8, 9, 6, 10, 11, 15]
  , testCase "Int DupN NoOp" $ view int (instructionIntDupN emptyState) @?= []
  , testCase "Int Yank Success" $ view int (instructionIntYank exampleState) @?= [6, 5, 8, 9, 10, 11, 15]
  , testCase "Int Yank NoOp" $ view int (instructionIntYank emptyState) @?= []
  , testCase "Int Shove Success" $ view int (instructionIntShove exampleState) @?= [8, 9, 5, 6, 10, 11, 15]
  , testCase "Int Shove NoOp" $ view int (instructionIntShove emptyState) @?= []
  , testCase "Int ShoveDup Success" $ view int (instructionIntShoveDup exampleState) @?= [5, 8, 9, 5, 6, 10, 11, 15]
  , testCase "Int ShoveDup NoOp" $ view int (instructionIntShoveDup emptyState) @?= []
  , testCase "Int DupItems Success" $ view int (instructionIntDupItems exampleState) @?= [5, 8, 9, 5, 8, 9, 6, 10, 11, 15]
  ]
