import Control.Exception (assert)
import GP
import Push

intTestFunc :: String -> [Int] -> [Gene] -> State -> IO ()
intTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == int (interpretExec state)) putStrLn (name ++ " passed test.")


main :: IO ()
main = do
  intTestFunc "instructionIntAdd" [8] [IntGene 6, IntGene 2, StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionIntSub" [4] [IntGene 6, IntGene 2, StateFunc instructionIntSub] emptyState
  intTestFunc "instructionIntMul" [12] [IntGene 6, IntGene 2, StateFunc instructionIntMul] emptyState
  intTestFunc "instructionIntDiv" [3] [IntGene 6, IntGene 2, StateFunc instructionIntDiv] emptyState
  intTestFunc "instructionExecIf" [6, 5] [BoolGene True, StateFunc instructionExecIf, Block [IntGene 5, IntGene 6], Block [IntGene 7, IntGene 8]] emptyState
  intTestFunc "instructionExecDup" [8] [StateFunc instructionExecDup, IntGene 4, StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionExecDoRange" [12] [IntGene 2, Block [IntGene 4, IntGene 1, StateFunc instructionExecDoRange], StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionExecDoCount" [8] [IntGene 2, Block [IntGene 4, StateFunc instructionExecDoCount], StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionIntAdd" [69, 69, 69, 69, 2] [IntGene 2, Block [IntGene 4, StateFunc instructionExecDoTimes], IntGene 69] emptyState
  intTestFunc "instructionExecDoTimes" [70, 70] [BoolGene False, BoolGene True, BoolGene True, StateFunc instructionExecWhile, IntGene 70] emptyState
  intTestFunc "instructionExecWhile" [70, 70, 70] [BoolGene False, BoolGene True, BoolGene True, StateFunc instructionExecDoWhile, IntGene 70] emptyState
  intTestFunc "instructionExecDoWhile" [71] [BoolGene True, StateFunc instructionExecWhen, IntGene 71] emptyState

  let loadedState = loadProgram [BoolGene False, StateFunc instructionExecWhen, IntGene 71] emptyState
  assert (emptyState == interpretExec loadedState) putStrLn "instructionExecWhen passed test."
