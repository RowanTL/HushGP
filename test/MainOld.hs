import Control.Exception (assert)
import Instructions
import Push
import State

-- import Debug.Trace

-- TODO: Need a function that can compare states.
-- May look at quickCheck later

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

stringTestFunc :: String -> [String] -> [Gene] -> State -> IO ()
stringTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _string (interpretExec state)) putStrLn (name <> " passed test.")

charTestFunc :: String -> [Char] -> [Gene] -> State -> IO ()
charTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _char (interpretExec state)) putStrLn (name <> " passed test.")

vectorIntTestFunc :: String -> [[Int]] -> [Gene] -> State -> IO ()
vectorIntTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _vectorInt (interpretExec state)) putStrLn (name <> " passed test.")

vectorFloatTestFunc :: String -> [[Float]] -> [Gene] -> State -> IO ()
vectorFloatTestFunc name goal genome startState =
  let state = loadProgram genome startState
   in assert (goal == _vectorFloat (interpretExec state)) putStrLn (name <> " passed test.")

main :: IO ()
main = do
  -- Int tests
  intTestFunc "instructionIntAdd" [8] [GeneInt 6, GeneInt 2, StateFunc (instructionIntAdd, "placeholder")] emptyState
  intTestFunc "instructionIntSub" [4] [GeneInt 6, GeneInt 2, StateFunc (instructionIntSub, "placeholder")] emptyState
  intTestFunc "instructionIntMul" [12] [GeneInt 6, GeneInt 2, StateFunc (instructionIntMul, "placeholder")] emptyState
  intTestFunc "instructionIntDiv" [3] [GeneInt 6, GeneInt 2, StateFunc (instructionIntDiv, "placeholder")] emptyState
  intTestFunc "instructionIntDiv0" [0, 2] [GeneInt 2, GeneInt 0, StateFunc (instructionIntDiv, "placeholder")] emptyState
  intTestFunc "instructionIntMod" [3] [GeneInt 13, GeneInt 5, StateFunc (instructionIntMod, "placeholder")] emptyState
  intTestFunc "instructionIntPop" [2] [GeneInt 2, GeneInt 0, StateFunc (instructionIntPop, "placeholder")] emptyState
  intTestFunc "instructionIntDup" [3, 3, 2] [GeneInt 2, GeneInt 3, StateFunc (instructionIntDup, "placeholder")] emptyState
  intTestFunc "instructionIntDupN3" [2, 2, 2] [GeneInt 2, GeneInt 3, StateFunc (instructionIntDupN, "placeholder")] emptyState
  intTestFunc "instructionIntDupN-1" [0] [GeneInt 0, GeneInt 2, GeneInt (-1), StateFunc (instructionIntDupN, "placeholder")] emptyState
  intTestFunc "instructionIntSwap" [2, 0, 3] [GeneInt 3, GeneInt 2, GeneInt 0, StateFunc (instructionIntSwap, "placeholder")] emptyState
  intTestFunc "instructionIntSwapFail" [1] [GeneInt 1, StateFunc (instructionIntSwap, "placeholder")] emptyState
  intTestFunc "instructionIntRot" [1, 3, 2] [GeneInt 1, GeneInt 2, GeneInt 3, StateFunc (instructionIntRot, "placeholder")] emptyState
  intTestFunc "instructionIntRotFail" [7, 8] [GeneInt 8, GeneInt 7, StateFunc (instructionIntRot, "placeholder")] emptyState
  intTestFunc "instructionIntFlush" [] [GeneInt 9696, GeneInt 92, GeneInt 420, StateFunc (instructionIntFlush, "placeholder")] emptyState -- I think I'm funny
  intTestFunc "instructionIntStackDepth" [2, 51, 52] [GeneInt 52, GeneInt 51, StateFunc (instructionIntStackDepth, "placeholder")] emptyState
  intTestFunc "instructionIntYank" [3, 3, 2, 1] [GeneInt 3, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, StateFunc (instructionIntYank, "placeholder")] emptyState
  intTestFunc "instructionIntYankDup" [3, 3, 2, 1, 3] [GeneInt 3, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, StateFunc (instructionIntYankDup, "placeholder")] emptyState
  intTestFunc "instructionIntShove" [2, 3, 1, 1] [GeneInt 1, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 2, StateFunc (instructionIntShove, "placeholder")] emptyState
  intTestFunc "instructionIntShoveDup" [3, 2, 3, 1, 1] [GeneInt 1, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 2, StateFunc (instructionIntShoveDup, "placeholder")] emptyState

  -- Exec tests
  intTestFunc "instructionExecIf" [6, 5] [GeneBool True, StateFunc (instructionExecIf, "placeholder"), Block [GeneInt 5, GeneInt 6], Block [GeneInt 7, GeneInt 8]] emptyState
  intTestFunc "instructionExecDup" [8] [StateFunc (instructionExecDup, "placeholder"), GeneInt 4, StateFunc (instructionIntAdd, "placeholder")] emptyState
  intTestFunc "instructionExecDoRange" [12] [GeneInt 2, Block [GeneInt 4, GeneInt 1, StateFunc (instructionExecDoRange, "placeholder")], StateFunc (instructionIntAdd, "placeholder")] emptyState
  intTestFunc "instructionExecDoCount" [8] [GeneInt 2, Block [GeneInt 4, StateFunc (instructionExecDoCount, "placeholder")], StateFunc (instructionIntAdd, "placeholder")] emptyState
  intTestFunc "instructionExecDoTimes" [69, 69, 69, 69, 2] [GeneInt 2, Block [GeneInt 4, StateFunc (instructionExecDoTimes, "placeholder")], GeneInt 69] emptyState
  intTestFunc "instructionExecWhile" [70, 70] [GeneBool False, GeneBool True, GeneBool True, StateFunc (instructionExecWhile, "placeholder"), GeneInt 70] emptyState
  intTestFunc "instructionExecDoWhile" [70, 70, 70] [GeneBool False, GeneBool True, GeneBool True, StateFunc (instructionExecDoWhile, "placeholder"), GeneInt 70] emptyState
  intTestFunc "instructionExecWhenTrue" [71] [GeneBool True, StateFunc (instructionExecWhen, "placeholder"), GeneInt 71] emptyState

  let loadedState = loadProgram [GeneBool False, StateFunc (instructionExecWhen, "placeholder"), GeneInt 71] emptyState
  assert (emptyState == interpretExec loadedState) putStrLn "instructionExecWhenFalse passed test."

  -- Float tests
  floatTestFunc "instructionFloatAdd" [4.32] [GeneFloat 4.01, GeneFloat 0.31, StateFunc (instructionFloatAdd, "placeholder")] emptyState
  floatTestFunc "instructionFloatSub" [3.6900003] [GeneFloat 4.01, GeneFloat 0.32, StateFunc (instructionFloatSub, "placeholder")] emptyState
  floatTestFunc "instructionFloatMul" [1.38] [GeneFloat 0.12, GeneFloat 11.5, StateFunc (instructionFloatMul, "placeholder")] emptyState
  floatTestFunc "instructionFloatDiv" [57.5] [GeneFloat 11.5, GeneFloat 0.2, StateFunc (instructionFloatDiv, "placeholder")] emptyState
  floatTestFunc "instructionFloatDiv0" [0, 69.69] [GeneFloat 69.69, GeneFloat 0.0, StateFunc (instructionFloatDiv, "placeholder")] emptyState
  floatTestFunc "instructionFloatYank" [1.1, 4.4, 3.3, 2.2] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc (instructionFloatYank, "placeholder")] emptyState
  floatTestFunc "instructionFloatYankDup" [1.1, 4.4, 3.3, 2.2, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc (instructionFloatYankDup, "placeholder")] emptyState
  floatTestFunc "instructionFloatShove" [3.3, 2.2, 4.4, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc (instructionFloatShove, "placeholder")] emptyState
  floatTestFunc "instructionFloatShoveDup" [4.4, 3.3, 2.2, 4.4, 1.1] [GeneInt 3, GeneFloat 1.1, GeneFloat 2.2, GeneFloat 3.3, GeneFloat 4.4, StateFunc (instructionFloatShoveDup, "placeholder")] emptyState
  floatTestFunc "instructionFloatDupNonEmpty" [4.4, 4.4, 3.3] [GeneFloat 3.3, GeneFloat 4.4, StateFunc (instructionFloatDup, "placeholder")] emptyState
  floatTestFunc "instructionFloatDupEmpty" [] [StateFunc (instructionFloatDup, "placeholder")] emptyState
  floatTestFunc "instructionFloatDupN3" [4.4, 4.4, 4.4, 3.3] [GeneFloat 3.3, GeneFloat 4.4, GeneInt 3, StateFunc (instructionFloatDupN, "placeholder")] emptyState
  floatTestFunc "instructionFloatDupN-1" [3.3] [GeneFloat 3.3, GeneFloat 4.4, GeneInt (-1), StateFunc (instructionFloatDupN, "placeholder")] emptyState
  boolTestFunc "instructionIntEqTrue" [True] [GeneInt 3, GeneInt 3, StateFunc (instructionIntEq, "placeholder")] emptyState
  boolTestFunc "instructionIntEqFalse" [False] [GeneInt 3, GeneInt 5, StateFunc (instructionIntEq, "placeholder")] emptyState
  boolTestFunc "instructionIntEqFail" [] [GeneInt 3, StateFunc (instructionIntEq, "placeholder")] emptyState

  -- Code tests
  codeTestFunc "instructionCodeFromExec" [] [StateFunc (instructionCodeFromExec, "placeholder"), StateFunc (instructionFloatFromInt, "placeholder"), StateFunc (instructionCodePop, "placeholder")] emptyState
  intTestFunc "instructionCodeDoRange" [18] [GeneInt 3, GeneInt 6, StateFunc (instructionCodeFromExec, "placeholder"), StateFunc (instructionIntAdd, "placeholder"), StateFunc (instructionCodeDoRange, "placeholder")] emptyState
  -- How to test instructionCodeDoThenPop?????
  codeTestFunc "instructionCodeFirst" [GeneInt 5] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 5, StateFunc (instructionIntSub, "placeholder")], StateFunc (instructionCodeFirst, "placeholder")] emptyState
  codeTestFunc "instructionCodeLast" [GeneBool True] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 5, StateFunc (instructionIntSub, "placeholder"), GeneBool True], StateFunc (instructionCodeLast, "placeholder")] emptyState
  codeTestFunc "instructionCodeTail" [Block [GeneFloat 3.2, GeneBool True, GeneInt 3]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [StateFunc (instructionFloatAdd, "placeholder"), GeneFloat 3.2, GeneBool True, GeneInt 3], StateFunc (instructionCodeTail, "placeholder")] emptyState
  codeTestFunc "instructionCodeInit" [Block [GeneVectorInt [1], GeneFloat 3.2, GeneBool True]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneVectorInt [1], GeneFloat 3.2, GeneBool True, GeneInt 3], StateFunc (instructionCodeInit, "placeholder")] emptyState
  codeTestFunc "instructionCodeWrap" [Block [GeneInt 3]] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeWrap, "placeholder")] emptyState
  codeTestFunc "instructionCodeList" [Block [GeneFloat 5.43, GeneInt 3]] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeFromExec, "placeholder"), GeneFloat 5.43, StateFunc (instructionCodeList, "placeholder")] emptyState
  codeTestFunc "instructionCodeCombine2Blocks" [Block [GeneInt 3, GeneInt 4, GeneInt 1, GeneInt 2]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2], StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 3, GeneInt 4], StateFunc (instructionCodeCombine, "placeholder")] emptyState
  codeTestFunc "instructionCodeCombine1Block1Single" [Block [GeneInt 3, GeneInt 4, GeneInt 1]] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 3, GeneInt 4], StateFunc (instructionCodeCombine, "placeholder")] emptyState
  codeTestFunc "instructionCodeCombine1Single1Block" [Block [GeneInt 3, GeneInt 1, GeneInt 2]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2], StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeCombine, "placeholder")] emptyState
  codeTestFunc "instrucitonCodeCombine2Single" [Block [GeneInt 2, GeneInt 1]] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 2, StateFunc (instructionCodeCombine, "placeholder")] emptyState
  intTestFunc "instructionCodeDo" [3] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeDo, "placeholder")] emptyState
  -- How to test instructionCodeDoDup??? We would would need a multi stack testing function
  boolTestFunc "instructionCodeIsCodeBlockTrue" [True] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 0], StateFunc (instructionCodeIsCodeBlock, "placeholder")] emptyState
  boolTestFunc "instructionCodeIsCodeBlockFalse" [False] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 0, StateFunc (instructionCodeIsCodeBlock, "placeholder")] emptyState
  boolTestFunc "instructionCodeIsSingularTrue" [True] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 0, StateFunc (instructionCodeIsSingular, "placeholder")] emptyState
  boolTestFunc "instructionCodeIsSingularFalse" [False] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 0], StateFunc (instructionCodeIsSingular, "placeholder")] emptyState
  intTestFunc "instructionCodeDoCount" [15] [GeneInt 6, StateFunc (instructionCodeFromExec, "placeholder"), StateFunc (instructionIntAdd, "placeholder"), StateFunc (instructionCodeDoCount, "placeholder")] emptyState
  intTestFunc "instructionCodeDoTimes" [13] [GeneInt 6, GeneInt 3, GeneInt 4, GeneInt 2, StateFunc (instructionCodeFromExec, "placeholder"), StateFunc (instructionIntAdd, "placeholder"), StateFunc (instructionCodeDoTimes, "placeholder")] emptyState
  intTestFunc "instructionCodeIfTrue" [6] [GeneBool True, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 6, StateFunc (instructionCodeIf, "placeholder")] emptyState
  intTestFunc "instructionCodeIfFalse" [3] [GeneBool False, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 3, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 6, StateFunc (instructionCodeIf, "placeholder")] emptyState
  intTestFunc "instructionCodeWhen" [6, 3, 6] [GeneInt 6, GeneInt 3, GeneInt 4, GeneInt 2, GeneBool True, StateFunc (instructionCodeFromExec, "placeholder"), StateFunc (instructionIntAdd, "placeholder"), StateFunc (instructionCodeWhen, "placeholder")] emptyState
  boolTestFunc "instructionCodeMemberTrue" [True] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 2, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc (instructionCodeMember, "placeholder")] emptyState
  boolTestFunc "instructionCodeMemberFalse" [False] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 7, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc (instructionCodeMember, "placeholder")] emptyState
  boolTestFunc "instructionCodeMember2Blocks" [False] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 7, GeneInt 0], StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc (instructionCodeMember, "placeholder")] emptyState
  codeTestFunc "instructionCodeNInBounds" [GeneInt 0] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 0, StateFunc (instructionCodeN, "placeholder")] emptyState
  codeTestFunc "instructionCodeNInBoundsEnd" [GeneInt 5] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 5, StateFunc (instructionCodeN, "placeholder")] emptyState
  codeTestFunc "instructionCodeNModded" [GeneInt 3] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 0, GeneInt 1, GeneInt 2, GeneInt 3, GeneInt 4, GeneInt 5], GeneInt 9, StateFunc (instructionCodeN, "placeholder")] emptyState
  codeTestFunc "instructionMakeEmptyCodeBlock" [Block []] [StateFunc (instructionMakeEmptyCodeBlock, "placeholder")] emptyState
  boolTestFunc "instructionIsEmptyCodeBlockTrue" [True] [StateFunc (instructionCodeFromExec, "placeholder"), Block [], StateFunc (instructionIsEmptyCodeBlock, "placeholder")] emptyState
  intTestFunc "instructionCodeSize" [8] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], StateFunc (instructionCodeSize, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractInBounds" [GeneInt 3] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], GeneInt 3, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractOutBounds" [GeneInt 3] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6], GeneInt 11, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractLastEmptyBlock" [Block []] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 7, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractBlock" [Block [GeneInt 2, GeneInt 3]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 1, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractEdgeCase" [Block []] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 7, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc "instructionCodeExtractNotBlock" [GeneInt 2] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 2, GeneInt 56, StateFunc (instructionCodeExtract, "placeholder")] emptyState
  codeTestFunc
    "instructionCodeInsertInBounds"
    [Block [GeneInt 1, Block [GeneInt 2, GeneInt 9999, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9]]
    [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 9999, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9], GeneInt 3, StateFunc (instructionCodeInsert, "placeholder")]
    emptyState
  codeTestFunc
    "instructionCodeInsertOutBounds"
    [Block [GeneInt 1, Block [GeneInt 2, GeneInt 9999, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9]]
    [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 9999, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9], GeneInt 15, StateFunc (instructionCodeInsert, "placeholder")]
    emptyState
  codeTestFunc "instructionCodeInsertNotBlock" [Block [GeneInt 2, GeneInt 1]] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 2, StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, GeneInt 1, StateFunc (instructionCodeInsert, "placeholder")] emptyState
  intTestFunc "instructionCodePosition0" [0] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc (instructionCodeFirstPosition, "placeholder")] emptyState
  intTestFunc "instructionCodePosition-1" [-1] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 7, StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc (instructionCodeFirstPosition, "placeholder")] emptyState
  intTestFunc "instructionCodePositionEmptyBlock" [0] [StateFunc (instructionCodeFromExec, "placeholder"), Block [], StateFunc (instructionCodeFromExec, "placeholder"), Block [], StateFunc (instructionCodeFirstPosition, "placeholder")] emptyState
  codeTestFunc "instructionCodePositionBadStack" [GeneInt 1] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, StateFunc (instructionCodeFirstPosition, "placeholder")] emptyState -- tests to ensure base case of insufficient code stack works. Should do this on more of these instructions.
  codeTestFunc "instructionCodeReverse2Args" [Block [GeneInt 2, GeneInt 1]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2], StateFunc (instructionCodeReverse, "placeholder")] emptyState
  codeTestFunc "instructionCodeReverse3Args" [Block [GeneInt 3, GeneInt 2, GeneInt 1]] [StateFunc (instructionCodeFromExec, "placeholder"), Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc (instructionCodeReverse, "placeholder")] emptyState
  codeTestFunc "instructionCodeReverseNonBlock" [GeneInt 1] [StateFunc (instructionCodeFromExec, "placeholder"), GeneInt 1, StateFunc (instructionCodeReverse, "placeholder")] emptyState

  -- String tests
  stringTestFunc "instructionStringConcat" ["123abc"] [GeneString "abc", GeneString "123", StateFunc (instructionStringConcat, "placeholder")] emptyState
  stringTestFunc "instructionStringSwap" ["abc", "123"] [GeneString "abc", GeneString "123", StateFunc (instructionStringSwap, "placeholder")] emptyState
  stringTestFunc "instructionStringInsertString" ["123INSabc"] [GeneString "abc", GeneString "123", StateFunc (instructionStringConcat, "placeholder"), GeneString "INS", StateFunc (instructionStringSwap, "placeholder"), GeneInt 3, StateFunc (instructionStringInsertString, "placeholder")] emptyState
  stringTestFunc "instructionStringFromFirstChar" ["1"] [GeneString "123", StateFunc (instructionStringFromFirstChar, "placeholder")] emptyState
  stringTestFunc "instructionStringFromNthChar" ["a"] [GeneString "123abc", GeneInt 3, StateFunc (instructionStringFromNthChar, "placeholder")] emptyState
  intTestFunc "instructionStringIndexOfString3" [3] [GeneString "a", GeneString "123abc", StateFunc (instructionStringIndexOfString, "placeholder")] emptyState
  intTestFunc "instructionStringIndexOfString-1" [-1] [GeneString "z", GeneString "123abc", StateFunc (instructionStringIndexOfString, "placeholder")] emptyState
  boolTestFunc "instructionStringContainsStringTrue" [True] [GeneString "a", GeneString "123abc", StateFunc (instructionStringContainsString, "placeholder")] emptyState
  boolTestFunc "instructionStringContainsStringFalse" [False] [GeneString "z", GeneString "123abc", StateFunc (instructionStringContainsString, "placeholder")] emptyState
  stringTestFunc "instructionStringSplitOnStringMult" ["nd", "r fri", "llo gam", "h"] [GeneString "e", GeneString "hello gamer friend", StateFunc (instructionStringSplitOnString, "placeholder")] emptyState
  stringTestFunc "instructionStringSplitOnStringEmpty" ["", "hello gamer frien"] [GeneString "d", GeneString "hello gamer friend", StateFunc (instructionStringSplitOnString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceFirstStringSuccess" ["thREPLACEs is a sentence"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence", StateFunc (instructionStringReplaceFirstString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceFirstStringFail" ["this is a sentence"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence", StateFunc (instructionStringReplaceFirstString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceNStringSuccess" ["thREPLACEs REPLACEs a sentence i"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceNString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceNStringFail" ["this is a sentence i"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceNString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceAllStringSuccess" ["thREPLACEs REPLACEs a sentence REPLACE"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceAllString, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceAllStringFail" ["this is a sentence i"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceAllString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveFirstStringSuccess" ["ths is a sentence"] [GeneString "i", GeneString "this is a sentence", StateFunc (instructionStringRemoveFirstString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveFirstStringFail" ["this is a sentence"] [GeneString "z", GeneString "this is a sentence", StateFunc (instructionStringRemoveFirstString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveNStringSuccess" ["ths s a sentence i"] [GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringRemoveNString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveNStringFail" ["this is a sentence i"] [GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringRemoveNString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveAllStringSuccess" ["ths s a sentence "] [GeneString "i", GeneString "this is a sentence i", StateFunc (instructionStringRemoveAllString, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveAllStringFail" ["this is a sentence i"] [GeneString "z", GeneString "this is a sentence i", StateFunc (instructionStringRemoveAllString, "placeholder")] emptyState
  intTestFunc "instructionStringOccurrencesOfString3" [3] [GeneString "i", GeneString "this is a sentence i", StateFunc (instructionStringOccurrencesOfString, "placeholder")] emptyState
  intTestFunc "instructionStringOccurrencesOfString3" [2] [GeneString "is", GeneString "this is a sentence i", StateFunc (instructionStringOccurrencesOfString, "placeholder")] emptyState
  intTestFunc "instructionStringOccurrencesOfString0" [0] [GeneString "z", GeneString "this is a sentence i", StateFunc (instructionStringOccurrencesOfString, "placeholder")] emptyState
  stringTestFunc "instructionStringInsertChar" ["123Zabc"] [GeneString "abc", GeneString "123", StateFunc (instructionStringConcat, "placeholder"), GeneChar 'Z', GeneInt 3, StateFunc (instructionStringInsertChar, "placeholder")] emptyState
  boolTestFunc "instructionStringContainsCharTrue" [True] [GeneString "abc", GeneChar 'a', StateFunc (instructionStringContainsChar, "placeholder")] emptyState
  boolTestFunc "instructionStringContainsCharFalse" [False] [GeneString "abc", GeneChar 'z', StateFunc (instructionStringContainsChar, "placeholder")] emptyState
  intTestFunc "instructionStringIndexOfChar3" [3] [GeneChar 'a', GeneString "123abc", StateFunc (instructionStringIndexOfChar, "placeholder")] emptyState
  intTestFunc "instructionStringIndexOfChar-1" [-1] [GeneChar 'z', GeneString "123abc", StateFunc (instructionStringIndexOfChar, "placeholder")] emptyState
  stringTestFunc "instructionStringSplitOnCharMult" ["nd", "r fri", "llo gam", "h"] [GeneChar 'e', GeneString "hello gamer friend", StateFunc (instructionStringSplitOnChar, "placeholder")] emptyState
  stringTestFunc "instructionStringSplitOnCharEmpty" ["", "hello gamer frien"] [GeneChar 'd', GeneString "hello gamer friend", StateFunc (instructionStringSplitOnChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceFirstCharSuccess" ["thRs is a sentence"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence", StateFunc (instructionStringReplaceFirstChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceFirstCharFail" ["this is a sentence"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence", StateFunc (instructionStringReplaceFirstChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceNCharSuccess" ["thRs Rs a sentence i"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceNChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceNCharFail" ["this is a sentence i"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceNChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceAllCharSuccess" ["thRs Rs a sentence R"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceAllChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReplaceAllCharFail" ["this is a sentence i"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringReplaceAllChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveFirstCharSuccess" ["ths is a sentence"] [GeneChar 'i', GeneString "this is a sentence", StateFunc (instructionStringRemoveFirstChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveFirstCharFail" ["this is a sentence"] [GeneChar 'z', GeneString "this is a sentence", StateFunc (instructionStringRemoveFirstChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveNCharSuccess" ["ths s a sentence i"] [GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringRemoveNChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveNCharFail" ["this is a sentence i"] [GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc (instructionStringRemoveNChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveAllCharSuccess" ["ths s a sentence "] [GeneChar 'i', GeneString "this is a sentence i", StateFunc (instructionStringRemoveAllChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveAllCharFail" ["this is a sentence i"] [GeneChar 'z', GeneString "this is a sentence i", StateFunc (instructionStringRemoveAllChar, "placeholder")] emptyState
  intTestFunc "instructionStringOccurrencesOfChar3" [3] [GeneChar 'i', GeneString "this is a sentence i", StateFunc (instructionStringOccurrencesOfChar, "placeholder")] emptyState
  intTestFunc "instructionStringOccurrencesOfChar0" [0] [GeneChar 'z', GeneString "this is a sentence i", StateFunc (instructionStringOccurrencesOfChar, "placeholder")] emptyState
  stringTestFunc "instructionStringReverse" ["321cba"] [GeneString "abc123", StateFunc (instructionStringReverse, "placeholder")] emptyState
  stringTestFunc "instructionStringHead3" ["abc"] [GeneString "abc123", GeneInt 3, StateFunc (instructionStringHead, "placeholder")] emptyState
  stringTestFunc "instructionStringHead0" [""] [GeneString "abc123", GeneInt 0, StateFunc (instructionStringHead, "placeholder")] emptyState
  stringTestFunc "instructionStringTail3" ["123"] [GeneString "abc123", GeneInt 3, StateFunc (instructionStringTail, "placeholder")] emptyState
  stringTestFunc "instructionStringTail0" [""] [GeneString "abc123", GeneInt 0, StateFunc (instructionStringTail, "placeholder")] emptyState
  stringTestFunc "instructionStringAppendChar" ["Rabc123"] [GeneString "abc123", GeneChar 'R', StateFunc (instructionStringAppendChar, "placeholder")] emptyState
  stringTestFunc "instructionStringRestFull" ["bc123"] [GeneString "abc123", StateFunc (instructionStringRest, "placeholder")] emptyState
  stringTestFunc "instructionStringRestEmpty" [""] [GeneString "", StateFunc (instructionStringRest, "placeholder")] emptyState
  stringTestFunc "instructionStringButLastFull" ["abc12"] [GeneString "abc123", StateFunc (instructionStringButLast, "placeholder")] emptyState
  stringTestFunc "instructionStringButLastEmpty" [""] [GeneString "", StateFunc (instructionStringButLast, "placeholder")] emptyState
  stringTestFunc "instructionStringDrop3" ["123"] [GeneString "abc123", GeneInt 3, StateFunc (instructionStringDrop, "placeholder")] emptyState
  stringTestFunc "instructionStringDrop0" ["abc123"] [GeneString "abc123", GeneInt 0, StateFunc (instructionStringDrop, "placeholder")] emptyState
  stringTestFunc "instructionStringButLastN3" ["abc"] [GeneString "abc123", GeneInt 3, StateFunc (instructionStringButLastN, "placeholder")] emptyState
  stringTestFunc "instructionStringButLastN0" ["abc123"] [GeneString "abc123", GeneInt 0, StateFunc (instructionStringButLastN, "placeholder")] emptyState
  intTestFunc "instructionStringLength6" [6] [GeneString "abc123", StateFunc (instructionStringLength, "placeholder")] emptyState
  stringTestFunc "instructionStringMakeEmpty" ["", "abc123"] [GeneString "abc123", StateFunc (instructionStringMakeEmpty, "placeholder")] emptyState
  stringTestFunc "instructionStringRemoveNth" ["abc23"] [GeneString "abc123", GeneInt 3, StateFunc (instructionStringRemoveNth, "placeholder")] emptyState
  stringTestFunc "instructionStringSetNth" ["abR123"] [GeneString "abc123", GeneInt 2, GeneChar 'R', StateFunc (instructionStringSetNth, "placeholder")] emptyState
  stringTestFunc "instructionStringStripWhitespace" ["abc123"] [GeneString " \r \n abc123 \t", StateFunc (instructionStringStripWhitespace, "placeholder")] emptyState
  stringTestFunc "instructionStringFromBoolTrue" ["True"] [GeneBool True, StateFunc (instructionStringFromBool, "placeholder")] emptyState
  stringTestFunc "instructionStringFromBoolTrue" ["False"] [GeneBool False, StateFunc (instructionStringFromBool, "placeholder")] emptyState
  stringTestFunc "instructionStringFromInt1000" ["1000"] [GeneInt 1000, StateFunc (instructionStringFromInt, "placeholder")] emptyState
  stringTestFunc "instructionStringFromInt-1" ["-1"] [GeneInt (-1), StateFunc (instructionStringFromInt, "placeholder")] emptyState
  stringTestFunc "instructionStringFromFloat3.2" ["3.2"] [GeneFloat 3.2, StateFunc (instructionStringFromFloat, "placeholder")] emptyState
  stringTestFunc "instructionStringFromFloat-99.0" ["-99.0"] [GeneFloat (-99.0), StateFunc (instructionStringFromFloat, "placeholder")] emptyState
  stringTestFunc "instructionStringFromChar" ["Z"] [GeneChar 'Z', StateFunc (instructionStringFromChar, "placeholder")] emptyState
  stringTestFunc "instructionStringFromChar" [" "] [GeneChar ' ', StateFunc (instructionStringFromChar, "placeholder")] emptyState

  -- char instructions
  stringTestFunc "instructionCharConcat" ["ab"] [GeneChar 'b', GeneChar 'a', StateFunc (instructionCharConcat, "placeholder")] emptyState
  charTestFunc "instructionCharFromFirstCharSuccess" ['a'] [GeneString "abc123", StateFunc (instructionCharFromFirstChar, "placeholder")] emptyState
  charTestFunc "instructionCharFromFirstCharFail" [] [GeneString "", StateFunc (instructionCharFromFirstChar, "placeholder")] emptyState
  charTestFunc "instructionCharFromLastCharSuccess" ['3'] [GeneString "abc123", StateFunc (instructionCharFromLastChar, "placeholder")] emptyState
  charTestFunc "instructionCharFromLastCharFail" [] [GeneString "", StateFunc (instructionCharFromLastChar, "placeholder")] emptyState
  charTestFunc "instructionCharFromNthCharSuccess" ['c'] [GeneString "abc123", GeneInt 2, StateFunc (instructionCharFromNthChar, "placeholder")] emptyState
  boolTestFunc "instructionCharIsWhitespaceSpace" [True] [GeneChar ' ', StateFunc (instructionCharIsWhitespace, "placeholder")] emptyState
  boolTestFunc "instructionCharIsWhitespacet" [True] [GeneChar '\t', StateFunc (instructionCharIsWhitespace, "placeholder")] emptyState
  boolTestFunc "instructionCharIsWhitespacer" [True] [GeneChar '\r', StateFunc (instructionCharIsWhitespace, "placeholder")] emptyState
  boolTestFunc "instructionCharIsWhitespacen" [True] [GeneChar '\n', StateFunc (instructionCharIsWhitespace, "placeholder")] emptyState
  boolTestFunc "instructionCharIsWhitespaceFalse" [False] [GeneChar 'a', StateFunc (instructionCharIsWhitespace, "placeholder")] emptyState
  boolTestFunc "instructionCharIsLetterTrue" [True] [GeneChar 'a', StateFunc (instructionCharIsLetter, "placeholder")] emptyState
  boolTestFunc "instructionCharIsLetterFalse" [False] [GeneChar '1', StateFunc (instructionCharIsLetter, "placeholder")] emptyState
  boolTestFunc "instructionCharIsDigitTrue" [True] [GeneChar '1', StateFunc (instructionCharIsDigit, "placeholder")] emptyState
  boolTestFunc "instructionCharIsDigitFalse" [False] [GeneChar 'a', StateFunc (instructionCharIsDigit, "placeholder")] emptyState

  -- vector int instructions
  vectorIntTestFunc "instructionVectorIntConcat" [[4, 5, 6, 1, 2, 3]] [GeneVectorInt [1, 2, 3], GeneVectorInt [4, 5, 6], StateFunc (instructionVectorIntConcat, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntConj" [[99, 1, 2, 3]] [GeneVectorInt [1, 2, 3], GeneInt 99, StateFunc (instructionVectorIntConj, "placeholder")] emptyState
  vectorIntTestFunc "instructionIntTakeN" [[1, 2], [6, 7, 8]] [GeneVectorInt [6, 7, 8], GeneVectorInt [1, 2, 3], GeneInt 2, StateFunc (instructionVectorIntTakeN, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntSubVector" [[1, 2, 3]] [GeneVectorInt [0, 1, 2, 3, 4, 5], GeneInt 3, GeneInt 1, StateFunc (instructionVectorIntSubVector, "placeholder")] emptyState
  intTestFunc "instructionVectorIntFirst" [1] [GeneVectorInt [1, 2, 3, 4, 5], StateFunc (instructionVectorIntFirst, "placeholder")] emptyState
  intTestFunc "instructionVectorIntLast" [5] [GeneVectorInt [1, 2, 3, 4, 5], StateFunc (instructionVectorIntLast, "placeholder")] emptyState
  intTestFunc "instructionVectorIntNthInBounds" [2] [GeneVectorInt [1, 2, 3, 4, 5], GeneInt 1, StateFunc (instructionVectorIntNth, "placeholder")] emptyState
  intTestFunc "instructionVectorIntNthOverflow" [2] [GeneVectorInt [1, 2, 3, 4, 5], GeneInt 6, StateFunc (instructionVectorIntNth, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntRestFull" [[2, 3, 4, 5]] [GeneVectorInt [1, 2, 3, 4, 5], StateFunc (instructionVectorIntRest, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntRestEmpty" [[]] [GeneVectorInt [], StateFunc (instructionVectorIntRest, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntButLastFull" [[1, 2, 3, 4]] [GeneVectorInt [1, 2, 3, 4, 5], StateFunc (instructionVectorIntButLast, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntButLastEmpty" [[]] [GeneVectorInt [], StateFunc (instructionVectorIntButLast, "placeholder")] emptyState
  intTestFunc "instructionVectorIntLength3" [3] [GeneVectorInt [1, 2, 3], StateFunc (instructionVectorIntLength, "placeholder")] emptyState
  intTestFunc "instructionVectorIntLength0" [0] [GeneVectorInt [], StateFunc (instructionVectorIntLength, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntReverse" [[4, 3, 2, 1]] [GeneVectorInt [1, 2, 3, 4], StateFunc (instructionVectorIntReverse, "placeholder")] emptyState
  intTestFunc "instructionVectorIntPushAllFull" [1, 2, 3, 4, 99] [GeneVectorInt [1, 2, 3, 4], GeneInt 99, StateFunc (instructionVectorIntPushAll, "placeholder")] emptyState
  intTestFunc "instructionVectorIntPushAllEmpty" [99] [GeneVectorInt [], GeneInt 99, StateFunc (instructionVectorIntPushAll, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntMakeEmpty" [[]] [StateFunc (instructionVectorIntMakeEmpty, "placeholder")] emptyState
  boolTestFunc "instructionVectorIntIsEmptyTrue" [True] [GeneVectorInt [], StateFunc (instructionVectorIntIsEmpty, "placeholder")] emptyState
  boolTestFunc "instructionVectorIntIsEmptyFalse" [False] [GeneVectorInt [1, 2, 3, 4], StateFunc (instructionVectorIntIsEmpty, "placeholder")] emptyState
  intTestFunc "instructionVectorIntIndexOf1" [1] [GeneVectorInt [1, 2, 3, 4, 5], GeneInt 2, StateFunc (instructionVectorIntIndexOf, "placeholder")] emptyState
  intTestFunc "instructionVectorIntIndexOfFail" [-1] [GeneVectorInt [], GeneInt 2, StateFunc (instructionVectorIntIndexOf, "placeholder")] emptyState
  intTestFunc "instructionVectorIntOccurrencesOf2" [2] [GeneVectorInt [1, 2, 3, 4, 2, 6, 7], GeneInt 2, StateFunc (instructionVectorIntOccurrencesOf, "placeholder")] emptyState
  intTestFunc "instructionVectorIntOccurrencesOf0" [0] [GeneVectorInt [1, 2, 3, 4, 2, 6, 7], GeneInt 0, StateFunc (instructionVectorIntOccurrencesOf, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntSetNth3" [[0, 1, 2, 99, 4, 5]] [GeneVectorInt [0, 1, 2, 3, 4, 5], GeneInt 99, GeneInt 3, StateFunc (instructionVectorIntSetNth, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntSetNth9" [[0, 1, 2, 99, 4, 5]] [GeneVectorInt [0, 1, 2, 3, 4, 5], GeneInt 99, GeneInt 9, StateFunc (instructionVectorIntSetNth, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntReplace3" [[0, 1, 2, 99, 4, 5, 99, 5, 99]] [GeneInt 99, GeneInt 3, GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntReplace, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntReplace-1" [[0, 1, 2, 3, 4, 5, 3, 5, 3]] [GeneInt 99, GeneInt (-1), GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntReplace, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntReplaceFirst3" [[0, 1, 2, 99, 4, 5, 3, 5, 3]] [GeneInt 99, GeneInt 3, GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntReplaceFirst, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntReplaceFirst-2" [[0, 1, 2, 3, 4, 5, 3, 5, 3]] [GeneInt 99, GeneInt (-2), GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntReplaceFirst, "placeholder")] emptyState
  vectorIntTestFunc "instructionVectorIntRemove" [[0, 1, 2, 4, 5, 5]] [GeneInt 3, GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntRemove, "placeholder")] emptyState
  intTestFunc "instructionVectorIntIterate" [66] [GeneInt 40, GeneVectorInt [0, 1, 2, 3, 4, 5, 3, 5, 3], StateFunc (instructionVectorIntIterate, "placeholder"), StateFunc (instructionIntAdd, "placeholder")] emptyState

  -- vector float functions
  vectorFloatTestFunc "instructionVectorFloatConcat" [[4.0, 5.0, 6.0, 1.0, 2.0, 3.0]] [GeneVectorFloat [1.0, 2.0, 3.0], GeneVectorFloat [4.0, 5.0, 6.0], StateFunc (instructionVectorFloatConcat, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatConj" [[99.0, 1.0, 2.0, 3.0]] [GeneVectorFloat [1.0, 2.0, 3.0], GeneFloat 99.0, StateFunc (instructionVectorFloatConj, "placeholder")] emptyState
  vectorFloatTestFunc "instructionFloatTakeN" [[1.0, 2.0], [6.0, 7.0, 8.0]] [GeneVectorFloat [6.0, 7.0, 8.0], GeneVectorFloat [1.0, 2.0, 3.0], GeneInt 2, StateFunc (instructionVectorFloatTakeN, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatSubVector" [[1.0, 2.0, 3.0]] [GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0], GeneInt 3, GeneInt 1, StateFunc (instructionVectorFloatSubVector, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatFirst" [1.0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], StateFunc (instructionVectorFloatFirst, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatLast" [5.0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], StateFunc (instructionVectorFloatLast, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatNthInBounds" [2.0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], GeneInt 1, StateFunc (instructionVectorFloatNth, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatNthOverflow" [2.0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], GeneInt 6, StateFunc (instructionVectorFloatNth, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatRestFull" [[2.0, 3.0, 4.0, 5.0]] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], StateFunc (instructionVectorFloatRest, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatRestEmpty" [[]] [GeneVectorFloat [], StateFunc (instructionVectorFloatRest, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatButLastFull" [[1.0, 2.0, 3.0, 4.0]] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], StateFunc (instructionVectorFloatButLast, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatButLastEmpty" [[]] [GeneVectorFloat [], StateFunc (instructionVectorFloatButLast, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatLength3" [3] [GeneVectorFloat [1.0, 2.0, 3.0], StateFunc (instructionVectorFloatLength, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatLength0" [0] [GeneVectorFloat [], StateFunc (instructionVectorFloatLength, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatReverse" [[4.0, 3.0, 2.0, 1.0]] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0], StateFunc (instructionVectorFloatReverse, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatPushAllFull" [1.0, 2.0, 3.0, 4.0, 99.0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0], GeneFloat 99.0, StateFunc (instructionVectorFloatPushAll, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatPushAllEmpty" [99.0] [GeneVectorFloat [], GeneFloat 99.0, StateFunc (instructionVectorFloatPushAll, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatMakeEmpty" [[]] [StateFunc (instructionVectorFloatMakeEmpty, "placeholder")] emptyState
  boolTestFunc "instructionVectorFloatIsEmptyTrue" [True] [GeneVectorFloat [], StateFunc (instructionVectorFloatIsEmpty, "placeholder")] emptyState
  boolTestFunc "instructionVectorFloatIsEmptyFalse" [False] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0], StateFunc (instructionVectorFloatIsEmpty, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatIndexOf1" [1] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 5.0], GeneFloat 2.0, StateFunc (instructionVectorFloatIndexOf, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatIndexOfFail" [-1] [GeneVectorFloat [], GeneFloat 2.0, StateFunc (instructionVectorFloatIndexOf, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatOccurrencesOf2" [2] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 2.0, 6.0, 7.0], GeneFloat 2.0, StateFunc (instructionVectorFloatOccurrencesOf, "placeholder")] emptyState
  intTestFunc "instructionVectorFloatOccurrencesOf0" [0] [GeneVectorFloat [1.0, 2.0, 3.0, 4.0, 2.0, 6.0, 7.0], GeneFloat 0.0, StateFunc (instructionVectorFloatOccurrencesOf, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatSetNth3" [[0.0, 1.0, 2.0, 99.0, 4.0, 5.0]] [GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0], GeneFloat 99.0, GeneInt 3, StateFunc (instructionVectorFloatSetNth, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatSetNth9" [[0.0, 1.0, 2.0, 99.0, 4.0, 5.0]] [GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0], GeneFloat 99.0, GeneInt 9, StateFunc (instructionVectorFloatSetNth, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatReplace3" [[0.0, 1.0, 2.0, 99.0, 4.0, 5.0, 99.0, 5.0, 99.0]] [GeneFloat 99.0, GeneFloat 3.0, GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatReplace, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatReplace-1" [[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0]] [GeneFloat 99.0, GeneFloat (-1.0), GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatReplace, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatReplaceFirst3" [[0.0, 1.0, 2.0, 99.0, 4.0, 5.0, 3.0, 5.0, 3.0]] [GeneFloat 99.0, GeneFloat 3.0, GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatReplaceFirst, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatReplaceFirst-2" [[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0]] [GeneFloat 99.0, GeneFloat (-2.0), GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatReplaceFirst, "placeholder")] emptyState
  vectorFloatTestFunc "instructionVectorFloatRemove" [[0.0, 1.0, 2.0, 4.0, 5.0, 5.0]] [GeneFloat 3, GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatRemove, "placeholder")] emptyState
  floatTestFunc "instructionVectorFloatIterate" [66.0] [GeneFloat 40.0, GeneVectorFloat [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 3.0, 5.0, 3.0], StateFunc (instructionVectorFloatIterate, "placeholder"), StateFunc (instructionFloatAdd, "placeholder")] emptyState
