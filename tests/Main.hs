import Control.Exception (assert)
import qualified Data.Map as Map
import Push
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

exampleState =
  State
    { exec = [IntGene 5, StateFunc instructionParameterLoad, StateFunc instructionIntAdd],
      int = [2, 6, 3],
      float = [1.2, 1.7],
      bool = [True, False],
      string = ["Hello", "Push"],
      parameter = [IntGene 1, StringGene "Hi", BoolGene True, FloatGene 1.3],
      input = Map.fromList [("in0", IntGene 1)]
    }

prop_test1 :: [Int] -> Bool
prop_test1 nums = nums == reverse (reverse nums)

main :: IO ()
main = do
  quickCheck prop_test1
  assert ([8, 3] == int (instructionIntAdd exampleState)) putStrLn "Add test pass"
  assert ([4, 3] == int (instructionIntSub exampleState)) putStrLn "Sub test pass"
  assert ([12, 3] == int (instructionIntMul exampleState)) putStrLn "Mult test pass"
  assert ([3, 3] == int (instructionIntDiv exampleState)) putStrLn "Div test pass"
  assert ([6, 2, 6, 3] == int (interpretExec exampleState)) putStrLn "Interpret test pass"

  let loadedState = loadProgram [IntGene 6, IntGene 6, StateFunc instructionIntAdd] emptyState
  assert ([12] == int (interpretExec loadedState)) putStrLn "Interpret test 2 pass"

  let loadedState2 = loadProgram [BoolGene True, StateFunc instructionExecIf, Block [IntGene 5, IntGene 6], Block [IntGene 7, IntGene 8]] emptyState
  assert ([6, 5] == int (interpretExec loadedState2)) putStrLn "execIf"

  let loadedState3 = loadProgram [BoolGene False, StateFunc instructionExecIf, Block [IntGene 5, IntGene 6], Block [IntGene 7, IntGene 8]] emptyState
  assert ([8, 7] == int (interpretExec loadedState3)) putStrLn "execIf"

  let loadedState4 = loadProgram [BoolGene False, PlaceInput "in0", StateFunc instructionIntAdd] exampleState
  assert ([3, 6, 3] == int (interpretExec loadedState4)) putStrLn "input map"

  let loadedState5 = interpretExec $ loadProgram [StateFunc instructionExecDup, IntGene 2] emptyState
  assert (int loadedState5 !! 0 == 2 && int loadedState5 !! 1 == 2) putStrLn "execDup"

  let loadedState6 = loadProgram [IntGene 2, Block [IntGene 4, IntGene 1, StateFunc instructionExecDoRange], StateFunc instructionIntAdd] emptyState
  assert ([12] == int (interpretExec loadedState6)) putStrLn "execDoRange"

  let loadedState7 = loadProgram [IntGene 2, Block [IntGene 4, StateFunc instructionExecDoCount], StateFunc instructionIntAdd] emptyState
  assert ([8] == int (interpretExec loadedState7)) putStrLn "execDoCount"

  let loadedState8 = loadProgram [IntGene 2, Block [IntGene 4, StateFunc instructionExecDoTimes], IntGene 69] emptyState
  assert ([69, 69, 69, 69, 2] == int (interpretExec loadedState8)) putStrLn "execDoTimes"

  let loadedState9 = loadProgram [BoolGene False, BoolGene True, BoolGene True, StateFunc instructionExecWhile, IntGene 70] emptyState
  assert ([70, 70] == int (interpretExec loadedState9)) putStrLn "execWhile"

  let loadedState10 = loadProgram [BoolGene False, BoolGene True, BoolGene True, StateFunc instructionExecDoWhile, IntGene 70] emptyState
  assert ([70, 70, 70] == int (interpretExec loadedState10)) putStrLn "execDoWhile"

  let loadedState11 = loadProgram [BoolGene False, StateFunc instructionExecWhen, IntGene 71] emptyState
  assert (emptyState == interpretExec loadedState11) putStrLn "execWhen"

  let loadedState12 = loadProgram [BoolGene True, StateFunc instructionExecWhen, IntGene 71] emptyState
  assert ([71] == int (interpretExec loadedState12)) putStrLn "execWhen"

  hspec $ do
    describe "Prelude.read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)

    describe "read" $ do
      it "is inverse to show" $
        property $
          \x -> (read . show) x `shouldBe` (x :: Int)
