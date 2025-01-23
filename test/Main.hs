import Control.Exception (assert)
import Instructions.CodeInstructions
import Instructions.ExecInstructions
import Instructions.FloatInstructions
import Instructions.IntInstructions
import Push
import State

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

codeTestFunc :: String -> [Gene] -> [Gene] -> State -> IO ()
codeTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _code (interpretExec state)) putStrLn (name <> " passed test.")

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
  intTestFunc "instructionIntYank" [3, 3, 2, 1] [GeneInt 3, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, StateFunc instructionIntYank] emptyState
  intTestFunc "instructionIntYankDup" [3, 3, 2, 1, 3] [GeneInt 3, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, StateFunc instructionIntYankDup] emptyState
  intTestFunc "instructionIntShove" [2, 1, 3, 1] [GeneInt 1, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 2, StateFunc instructionIntShove] emptyState
  intTestFunc "instructionIntShoveDup" [3, 2, 1, 3, 1] [GeneInt 1, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 2, StateFunc instructionIntShoveDup] emptyState

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
  floatTestFunc "instructionFloatYank" [1.1, 4.4, 3.3, 2.2] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc instructionFloatYank] emptyState
  floatTestFunc "instructionFloatYankDup" [1.1, 4.4, 3.3, 2.2, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc instructionFloatYankDup] emptyState
  floatTestFunc "instructionFloatShove" [3.3, 2.2, 4.4, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc instructionFloatShove] emptyState
  floatTestFunc "instructionFloatShoveDup" [4.4, 3.3, 2.2, 4.4, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc instructionFloatShoveDup] emptyState

  -- Bool tests
  boolTestFunc "instructionIntEqTrue" [True] [GeneInt 3, GeneInt 3, StateFunc instructionIntEq] emptyState
  boolTestFunc "instructionIntEqFalse" [False] [GeneInt 3, GeneInt 5, StateFunc instructionIntEq] emptyState
  boolTestFunc "instructionIntEqFail" [] [GeneInt 3, StateFunc instructionIntEq] emptyState

  -- Code tests
  codeTestFunc "instructionCodeFromExec" [] [StateFunc instructionCodeFromExec, StateFunc instructionFloatFromInt, StateFunc instructionCodePop] emptyState
  intTestFunc "instructionCodeDoRange" [18] [GeneInt 3, GeneInt 6, StateFunc instructionCodeFromExec, StateFunc instructionIntAdd, StateFunc instructionCodeDoRange] emptyState
  -- How to test instructionCodeDoThenPop?????
  codeTestFunc "instructionCodeFirst" [GeneInt 5] [StateFunc instructionCodeFromExec, Block [GeneInt 5, StateFunc instructionIntSub], StateFunc instructionCodeFirst] emptyState
  codeTestFunc "instructionCodeLast" [GeneBool True] [StateFunc instructionCodeFromExec, Block [GeneInt 5, StateFunc instructionIntSub, GeneBool True], StateFunc instructionCodeLast] emptyState
  codeTestFunc "instructionCodeTail" [Block [GeneFloat 3.2, GeneBool True, GeneInt 3]] [StateFunc instructionCodeFromExec, Block [StateFunc instructionFloatAdd, GeneFloat 3.2, GeneBool True, GeneInt 3], StateFunc instructionCodeTail] emptyState
  codeTestFunc "instructionCodeInit" [Block [GeneIntVector [1], GeneFloat 3.2, GeneBool True]] [StateFunc instructionCodeFromExec, Block [GeneIntVector [1], GeneFloat 3.2, GeneBool True, GeneInt 3], StateFunc instructionCodeInit] emptyState
  codeTestFunc "instructionCodeWrap" [Block [GeneInt 3]] [StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeWrap] emptyState
  codeTestFunc "instructionCodeList" [Block [GeneFloat 5.43, GeneInt 3]] [StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeFromExec, GeneFloat 5.43, StateFunc instructionCodeList] emptyState
  codeTestFunc "instructionCodeCombine2Blocks" [Block [GeneInt 3, GeneInt 4, GeneInt 1, GeneInt 2]] [StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2], StateFunc instructionCodeFromExec, Block [GeneInt 3, GeneInt 4], StateFunc instructionCodeCombine] emptyState
  codeTestFunc "instructionCodeCombine1Block1Single" [Block [GeneInt 3, GeneInt 4, GeneInt 1]] [StateFunc instructionCodeFromExec, GeneInt 1, StateFunc instructionCodeFromExec, Block [GeneInt 3, GeneInt 4], StateFunc instructionCodeCombine] emptyState
  codeTestFunc "instructionCodeCombine1Single1Block" [Block [GeneInt 3, GeneInt 1, GeneInt 2]] [StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2], StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeCombine] emptyState
  codeTestFunc "instrucitonCodeCombine2Single" [Block [GeneInt 2, GeneInt 1]] [StateFunc instructionCodeFromExec, GeneInt 1, StateFunc instructionCodeFromExec, GeneInt 2, StateFunc instructionCodeCombine] emptyState
  intTestFunc "instructionCodeDo" [3] [StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeDo] emptyState
  -- How to test instructionCodeDoDup??? We would would need a multi stack testing function
  boolTestFunc "instructionCodeIsCodeBlockTrue" [True] [StateFunc instructionCodeFromExec, Block [GeneInt 0], StateFunc instructionCodeIsCodeBlock] emptyState
  boolTestFunc "instructionCodeIsCodeBlockFalse" [False] [StateFunc instructionCodeFromExec, GeneInt 0, StateFunc instructionCodeIsCodeBlock] emptyState
  boolTestFunc "instructionCodeIsSingularTrue" [True] [StateFunc instructionCodeFromExec, GeneInt 0, StateFunc instructionCodeIsSingular] emptyState
  boolTestFunc "instructionCodeIsSingularFalse" [False] [StateFunc instructionCodeFromExec, Block [GeneInt 0], StateFunc instructionCodeIsSingular] emptyState
  intTestFunc "instructionCodeDoCount" [15] [GeneInt 6, StateFunc instructionCodeFromExec, StateFunc instructionIntAdd, StateFunc instructionCodeDoCount] emptyState
  intTestFunc "instructionCodeDoTimes" [13] [GeneInt 6, GeneInt 3, GeneInt 4, GeneInt 2, StateFunc instructionCodeFromExec, StateFunc instructionIntAdd, StateFunc instructionCodeDoTimes] emptyState
  intTestFunc "instructionCodeIfTrue" [6] [GeneBool True, StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeFromExec, GeneInt 6, StateFunc instructionCodeIf] emptyState
  intTestFunc "instructionCodeIfFalse" [3] [GeneBool False, StateFunc instructionCodeFromExec, GeneInt 3, StateFunc instructionCodeFromExec, GeneInt 6, StateFunc instructionCodeIf] emptyState
  intTestFunc "instructionCodeWhen" [6, 3, 6] [GeneInt 6, GeneInt 3, GeneInt 4, GeneInt 2, GeneBool True, StateFunc instructionCodeFromExec, StateFunc instructionIntAdd, StateFunc instructionCodeWhen] emptyState
  boolTestFunc "instructionCodeMemberTrue" [True] [StateFunc instructionCodeFromExec, GeneInt 2, StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneIntVector [8, 9]], StateFunc instructionCodeMember] emptyState
  boolTestFunc "instructionCodeMemberFalse" [False] [StateFunc instructionCodeFromExec, GeneInt 7, StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneIntVector [8, 9]], StateFunc instructionCodeMember] emptyState
  boolTestFunc "instructionCodeMember2Blocks" [False] [StateFunc instructionCodeFromExec, Block [GeneInt 7, GeneInt 0], StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneIntVector [8, 9]], StateFunc instructionCodeMember] emptyState
  codeTestFunc "instructionCodeNInBounds" [GeneInt 0] [StateFunc instructionCodeFromExec, Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 0, StateFunc instructionCodeN] emptyState
  codeTestFunc "instructionCodeNInBoundsEnd" [GeneInt 5] [StateFunc instructionCodeFromExec, Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 5, StateFunc instructionCodeN] emptyState
  codeTestFunc "instructionCodeNModded" [GeneInt 3] [StateFunc instructionCodeFromExec, Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 9, StateFunc instructionCodeN] emptyState
  codeTestFunc "instructionMakeEmptyCodeBlock" [Block []] [StateFunc instructionMakeEmptyCodeBlock] emptyState
  boolTestFunc "instructionIsEmptyCodeBlockTrue" [True] [StateFunc instructionCodeFromExec, Block [], StateFunc instructionIsEmptyCodeBlock] emptyState
  intTestFunc "instructionCodeSize" [8] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], StateFunc instructionCodeSize] emptyState
  codeTestFunc "instructionCodeExtractInBounds" [GeneInt 3] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], GeneInt 3, StateFunc instructionCodeExtract] emptyState
  codeTestFunc "instructionCodeExtractOutBounds" [GeneInt 3] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], GeneInt 11, StateFunc instructionCodeExtract] emptyState
  codeTestFunc "instructionCodeExtractLastEmptyBlock" [Block []] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 7, StateFunc instructionCodeExtract] emptyState
  codeTestFunc "instructionCodeExtractBlock" [Block [GeneInt 2, GeneInt 3]] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 1, StateFunc instructionCodeExtract] emptyState
