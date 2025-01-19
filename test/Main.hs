import Control.Exception (assert)
import Push
import State
import Instructions.IntInstructions
import Instructions.ExecInstructions
import Instructions.FloatInstructions

-- @TODO: Finish int and float tests

-- TODO: Need a function that can compare states.

intTestFunc :: String -> [Int] -> [Gene] -> State -> IO ()
intTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _int (interpretExec state)) putStrLn (name ++ " passed test.")

floatTestFunc :: String -> [Float] -> [Gene] -> State -> IO ()
floatTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _float (interpretExec state)) putStrLn (name ++ " passed test.")

boolTestFunc :: String -> [Bool] -> [Gene] -> State -> IO ()
boolTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _bool (interpretExec state)) putStrLn (name ++ " passed test.")

main :: IO ()
main = do
  -- Int tests
  intTestFunc "instructionIntAdd" [8] [GeneInt 6, GeneInt 2, StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionIntSub" [4] [GeneInt 6, GeneInt 2, StateFunc instructionIntSub] emptyState
  intTestFunc "instructionIntMul" [12] [GeneInt 6, GeneInt 2, StateFunc instructionIntMul] emptyState
  intTestFunc "instructionIntDiv" [3] [GeneInt 6, GeneInt 2, StateFunc instructionIntDiv] emptyState
  intTestFunc "instructionIntDiv0" [0, 2] [GeneInt 2, GeneInt 0, StateFunc instructionIntDiv] emptyState
  intTestFunc "instructionIntMod" [3] [GeneInt 13, GeneInt 5, StateFunc instructionIntMod] emptyState
  intTestFunc "instructionIntPop" [2] [GeneInt 2, GeneInt 0, StateFunc instructionIntPop] emptyState
  intTestFunc "instrucitonIntDup" [3, 3, 2] [GeneInt 2, GeneInt 3, StateFunc instructionIntDup] emptyState
  intTestFunc "instructionIntDupN" [2, 2, 2] [GeneInt 2, GeneInt 3, StateFunc instructionIntDupN] emptyState
  intTestFunc "instructionIntSwap" [2, 0, 3] [GeneInt 3, GeneInt 2, GeneInt 0, StateFunc instructionIntSwap] emptyState
  intTestFunc "instructionIntSwapFail" [1] [GeneInt 1, StateFunc instructionIntSwap] emptyState
  intTestFunc "instructionIntRot" [1, 3, 2] [GeneInt 1, GeneInt 2, GeneInt 3, StateFunc instructionIntRot] emptyState
  intTestFunc "instructionIntRotFail" [7, 8] [GeneInt 8, GeneInt 7, StateFunc instructionIntRot] emptyState
  intTestFunc "instructionIntFlush" [] [GeneInt 9696, GeneInt 92, GeneInt 420, StateFunc instructionIntFlush] emptyState -- I think I'm funny
  intTestFunc "instructionIntStackDepth" [2, 51, 52] [GeneInt 52, GeneInt 51, StateFunc instructionIntStackDepth] emptyState

  -- Exec tests
  intTestFunc "instructionExecIf" [6, 5] [GeneBool True, StateFunc instructionExecIf, Block [GeneInt 5, GeneInt 6], Block [GeneInt 7, GeneInt 8]] emptyState
  intTestFunc "instructionExecDup" [8] [StateFunc instructionExecDup, GeneInt 4, StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionExecDoRange" [12] [GeneInt 2, Block [GeneInt 4, GeneInt 1, StateFunc instructionExecDoRange], StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionExecDoCount" [8] [GeneInt 2, Block [GeneInt 4, StateFunc instructionExecDoCount], StateFunc instructionIntAdd] emptyState
  intTestFunc "instructionExecDoTimes" [69, 69, 69, 69, 2] [GeneInt 2, Block [GeneInt 4, StateFunc instructionExecDoTimes], GeneInt 69] emptyState
  intTestFunc "instructionExecWhile" [70, 70] [GeneBool False, GeneBool True, GeneBool True, StateFunc instructionExecWhile, GeneInt 70] emptyState
  intTestFunc "instructionExecDoWhile" [70, 70, 70] [GeneBool False, GeneBool True, GeneBool True, StateFunc instructionExecDoWhile, GeneInt 70] emptyState
  intTestFunc "instructionExecWhenTrue" [71] [GeneBool True, StateFunc instructionExecWhen, GeneInt 71] emptyState

  let loadedState = loadProgram [GeneBool False, StateFunc instructionExecWhen, GeneInt 71] emptyState
  assert (emptyState == interpretExec loadedState) putStrLn "instructionExecWhenFalse passed test."

  -- Float tests
  floatTestFunc "instructionFloatAdd" [4.32] [GeneFloat 4.01, GeneFloat 0.31, StateFunc instructionFloatAdd] emptyState 
  floatTestFunc "instructionFloatSub" [3.6900003] [GeneFloat 4.01, GeneFloat 0.32, StateFunc instructionFloatSub] emptyState
  floatTestFunc "instructionFloatMul" [1.38] [GeneFloat 0.12, GeneFloat 11.5, StateFunc instructionFloatMul] emptyState
  floatTestFunc "instructionFloatDiv" [57.5] [GeneFloat 11.5, GeneFloat 0.2, StateFunc instructionFloatDiv] emptyState
  floatTestFunc "instructionFloatDiv0" [0, 69.69] [GeneFloat 69.69, GeneFloat 0.0, StateFunc instructionFloatDiv] emptyState

  -- Bool tests
  boolTestFunc "instructionIntEqTrue" [True] [GeneInt 3, GeneInt 3, StateFunc instructionIntEq] emptyState
  boolTestFunc "instructionIntEqFalse" [False] [GeneInt 3, GeneInt 5, StateFunc instructionIntEq] emptyState
  boolTestFunc "instructionIntEqFail" [] [GeneInt 3, StateFunc instructionIntEq] emptyState
