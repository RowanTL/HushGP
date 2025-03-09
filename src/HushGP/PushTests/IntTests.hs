module HushGP.PushTests.IntTests where

import Data.Char
import HushGP.State
import HushGP.PushTests.GenericTests
import HushGP.Instructions.IntInstructions
-- import Control.Lens hiding (uncons)
import System.Environment
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "10"
  setEnv "TASTY_QUICKCHECK_VERBOSE" "False"
  defaultMain intTests

-- |Holds the tree for property and unit tests.
intTests :: TestTree
intTests = testGroup "All Int Tests" []
