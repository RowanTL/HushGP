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
  intTestFunc "instructionIntDup" [3, 3, 2] [GeneInt 2, GeneInt 3, StateFunc instructionIntDup] emptyState
  intTestFunc "instructionIntDupN3" [2, 2, 2] [GeneInt 2, GeneInt 3, StateFunc instructionIntDupN] emptyState
  intTestFunc "instructionIntDupN-1" [0] [GeneInt 0, GeneInt 2, GeneInt (-1), StateFunc instructionIntDupN] emptyState
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
  floatTestFunc "instructionFloatDupNonEmpty" [4.4, 4.4, 3.3] [GeneFloat 3.3, GeneFloat 4.4, StateFunc instructionFloatDup] emptyState
  floatTestFunc "instructionFloatDupEmpty" [] [StateFunc instructionFloatDup] emptyState
  floatTestFunc "instructionFloatDupN3" [4.4, 4.4, 4.4, 3.3] [GeneFloat 3.3, GeneFloat 4.4, GeneInt 3, StateFunc instructionFloatDupN] emptyState
  floatTestFunc "instructionFloatDupN-1" [3.3] [GeneFloat 3.3, GeneFloat 4.4, GeneInt (-1), StateFunc instructionFloatDupN] emptyState
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
  codeTestFunc "instructionCodeInit" [Block [GeneVectorInt [1], GeneFloat 3.2, GeneBool True]] [StateFunc instructionCodeFromExec, Block [GeneVectorInt [1], GeneFloat 3.2, GeneBool True, GeneInt 3], StateFunc instructionCodeInit] emptyState
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
  boolTestFunc "instructionCodeMemberTrue" [True] [StateFunc instructionCodeFromExec, GeneInt 2, StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc instructionCodeMember] emptyState
  boolTestFunc "instructionCodeMemberFalse" [False] [StateFunc instructionCodeFromExec, GeneInt 7, StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc instructionCodeMember] emptyState
  boolTestFunc "instructionCodeMember2Blocks" [False] [StateFunc instructionCodeFromExec, Block [GeneInt 7, GeneInt 0], StateFunc instructionCodeFromExec, Block [GeneFloat 3.6, GeneInt 2, GeneVectorInt [8, 9]], StateFunc instructionCodeMember] emptyState
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
  codeTestFunc "instructionCodeExtractEdgeCase" [Block []] [StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], Block []], GeneInt 7, StateFunc instructionCodeExtract] emptyState
  codeTestFunc "instructionCodeExtractNotBlock" [GeneInt 2] [StateFunc instructionCodeFromExec, GeneInt 2, GeneInt 56, StateFunc instructionCodeExtract] emptyState
  codeTestFunc
    "instructionCodeInsertInBounds"
    [Block [GeneInt 1, Block [GeneInt 2, GeneInt 9999, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9]]
    [StateFunc instructionCodeFromExec, GeneInt 9999, StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9], GeneInt 3, StateFunc instructionCodeInsert]
    emptyState
  codeTestFunc
    "instructionCodeInsertOutBounds"
    [Block [GeneInt 1, Block [GeneInt 2, GeneInt 9999, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9]]
    [StateFunc instructionCodeFromExec, GeneInt 9999, StateFunc instructionCodeFromExec, Block [GeneInt 1, Block [GeneInt 2, GeneInt 3], Block [GeneInt 4, GeneInt 5], GeneInt 6, Block [GeneInt 7, GeneInt 8], GeneInt 9], GeneInt 15, StateFunc instructionCodeInsert]
    emptyState
  codeTestFunc "instructionCodeInsertNotBlock" [Block [GeneInt 2, GeneInt 1]] [StateFunc instructionCodeFromExec, GeneInt 2, StateFunc instructionCodeFromExec, GeneInt 1, GeneInt 1, StateFunc instructionCodeInsert] emptyState
  intTestFunc "instructionCodePosition0" [0] [StateFunc instructionCodeFromExec, GeneInt 1, StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc instructionCodeFirstPosition] emptyState
  intTestFunc "instructionCodePosition-1" [-1] [StateFunc instructionCodeFromExec, GeneInt 7, StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc instructionCodeFirstPosition] emptyState
  intTestFunc "instructionCodePositionEmptyBlock" [0] [StateFunc instructionCodeFromExec, Block [], StateFunc instructionCodeFromExec, Block [], StateFunc instructionCodeFirstPosition] emptyState
  codeTestFunc "instructionCodePositionBadStack" [GeneInt 1] [StateFunc instructionCodeFromExec, GeneInt 1, StateFunc instructionCodeFirstPosition] emptyState -- tests to ensure base case of insufficient code stack works. Should do this on more of these instructions.
  codeTestFunc "instructionCodeReverse2Args" [Block [GeneInt 2, GeneInt 1]] [StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2], StateFunc instructionCodeReverse] emptyState
  codeTestFunc "instructionCodeReverse3Args" [Block [GeneInt 3, GeneInt 2, GeneInt 1]] [StateFunc instructionCodeFromExec, Block [GeneInt 1, GeneInt 2, GeneInt 3], StateFunc instructionCodeReverse] emptyState
  codeTestFunc "instructionCodeReverseNonBlock" [GeneInt 1] [StateFunc instructionCodeFromExec, GeneInt 1, StateFunc instructionCodeReverse] emptyState

  -- String tests
  stringTestFunc "instructionStringConcat" ["123abc"] [GeneString "abc", GeneString "123", StateFunc instructionStringConcat] emptyState
  stringTestFunc "instructionStringSwap" ["abc", "123"] [GeneString "abc", GeneString "123", StateFunc instructionStringSwap] emptyState
  stringTestFunc "instructionStringInsertString" ["123INSabc"] [GeneString "abc", GeneString "123", StateFunc instructionStringConcat, GeneString "INS", StateFunc instructionStringSwap, GeneInt 3, StateFunc instructionStringInsertString] emptyState
  stringTestFunc "instructionStringFromFirstChar" ["1"] [GeneString "123", StateFunc instructionStringFromFirstChar] emptyState
  stringTestFunc "instructionStringFromNthChar" ["a"] [GeneString "123abc", GeneInt 3, StateFunc instructionStringFromNthChar] emptyState
  intTestFunc "instructionStringIndexOfString3" [3] [GeneString "a", GeneString "123abc", StateFunc instructionStringIndexOfString] emptyState
  intTestFunc "instructionStringIndexOfString-1" [-1] [GeneString "z", GeneString "123abc", StateFunc instructionStringIndexOfString] emptyState
  boolTestFunc "instructionStringContainsStringTrue" [True] [GeneString "a", GeneString "123abc", StateFunc instructionStringContainsString] emptyState
  boolTestFunc "instructionStringContainsStringFalse" [False] [GeneString "z", GeneString "123abc", StateFunc instructionStringContainsString] emptyState
  stringTestFunc "instructionStringSplitOnStringMult" ["nd", "r fri", "llo gam", "h"] [GeneString "e", GeneString "hello gamer friend", StateFunc instructionStringSplitOnString] emptyState
  stringTestFunc "instructionStringSplitOnStringEmpty" ["", "hello gamer frien"] [GeneString "d", GeneString "hello gamer friend", StateFunc instructionStringSplitOnString] emptyState
  stringTestFunc "instructionStringReplaceFirstStringSuccess" ["thREPLACEs is a sentence"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence", StateFunc instructionStringReplaceFirstString] emptyState
  stringTestFunc "instructionStringReplaceFirstStringFail" ["this is a sentence"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence", StateFunc instructionStringReplaceFirstString] emptyState
  stringTestFunc "instructionStringReplaceNStringSuccess" ["thREPLACEs REPLACEs a sentence i"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceNString] emptyState
  stringTestFunc "instructionStringReplaceNStringFail" ["this is a sentence i"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceNString] emptyState
  stringTestFunc "instructionStringReplaceAllStringSuccess" ["thREPLACEs REPLACEs a sentence REPLACE"] [GeneString "REPLACE", GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceAllString] emptyState
  stringTestFunc "instructionStringReplaceAllStringFail" ["this is a sentence i"] [GeneString "REPLACE", GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceAllString] emptyState
  stringTestFunc "instructionStringRemoveFirstStringSuccess" ["ths is a sentence"] [GeneString "i", GeneString "this is a sentence", StateFunc instructionStringRemoveFirstString] emptyState
  stringTestFunc "instructionStringRemoveFirstStringFail" ["this is a sentence"] [GeneString "z", GeneString "this is a sentence", StateFunc instructionStringRemoveFirstString] emptyState
  stringTestFunc "instructionStringRemoveNStringSuccess" ["ths s a sentence i"] [GeneString "i", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringRemoveNString] emptyState
  stringTestFunc "instructionStringRemoveNStringFail" ["this is a sentence i"] [GeneString "z", GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringRemoveNString] emptyState
  stringTestFunc "instructionStringRemoveAllStringSuccess" ["ths s a sentence "] [GeneString "i", GeneString "this is a sentence i", StateFunc instructionStringRemoveAllString] emptyState
  stringTestFunc "instructionStringRemoveAllStringFail" ["this is a sentence i"] [GeneString "z", GeneString "this is a sentence i", StateFunc instructionStringRemoveAllString] emptyState
  intTestFunc "instructionStringOccurrencesOfString3" [3] [GeneString "i", GeneString "this is a sentence i", StateFunc instructionStringOccurrencesOfString] emptyState
  intTestFunc "instructionStringOccurrencesOfString3" [2] [GeneString "is", GeneString "this is a sentence i", StateFunc instructionStringOccurrencesOfString] emptyState
  intTestFunc "instructionStringOccurrencesOfString0" [0] [GeneString "z", GeneString "this is a sentence i", StateFunc instructionStringOccurrencesOfString] emptyState
  stringTestFunc "instructionStringInsertChar" ["123Zabc"] [GeneString "abc", GeneString "123", StateFunc instructionStringConcat, GeneChar 'Z', GeneInt 3, StateFunc instructionStringInsertChar] emptyState
  boolTestFunc "instructionStringContainsCharTrue" [True] [GeneString "abc", GeneChar 'a', StateFunc instructionStringContainsChar] emptyState
  boolTestFunc "instructionStringContainsCharFalse" [False] [GeneString "abc", GeneChar 'z', StateFunc instructionStringContainsChar] emptyState
  intTestFunc "instructionStringIndexOfChar3" [3] [GeneChar 'a', GeneString "123abc", StateFunc instructionStringIndexOfChar] emptyState
  intTestFunc "instructionStringIndexOfChar-1" [-1] [GeneChar 'z', GeneString "123abc", StateFunc instructionStringIndexOfChar] emptyState
  stringTestFunc "instructionStringSplitOnCharMult" ["nd", "r fri", "llo gam", "h"] [GeneChar 'e', GeneString "hello gamer friend", StateFunc instructionStringSplitOnChar] emptyState
  stringTestFunc "instructionStringSplitOnCharEmpty" ["", "hello gamer frien"] [GeneChar 'd', GeneString "hello gamer friend", StateFunc instructionStringSplitOnChar] emptyState
  stringTestFunc "instructionStringReplaceFirstCharSuccess" ["thRs is a sentence"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence", StateFunc instructionStringReplaceFirstChar] emptyState
  stringTestFunc "instructionStringReplaceFirstCharFail" ["this is a sentence"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence", StateFunc instructionStringReplaceFirstChar] emptyState
  stringTestFunc "instructionStringReplaceNCharSuccess" ["thRs Rs a sentence i"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceNChar] emptyState
  stringTestFunc "instructionStringReplaceNCharFail" ["this is a sentence i"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceNChar] emptyState
  stringTestFunc "instructionStringReplaceAllCharSuccess" ["thRs Rs a sentence R"] [GeneChar 'R', GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceAllChar] emptyState
  stringTestFunc "instructionStringReplaceAllCharFail" ["this is a sentence i"] [GeneChar 'R', GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringReplaceAllChar] emptyState
  stringTestFunc "instructionStringRemoveFirstCharSuccess" ["ths is a sentence"] [GeneChar 'i', GeneString "this is a sentence", StateFunc instructionStringRemoveFirstChar] emptyState
  stringTestFunc "instructionStringRemoveFirstCharFail" ["this is a sentence"] [GeneChar 'z', GeneString "this is a sentence", StateFunc instructionStringRemoveFirstChar] emptyState
  stringTestFunc "instructionStringRemoveNCharSuccess" ["ths s a sentence i"] [GeneChar 'i', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringRemoveNChar] emptyState
  stringTestFunc "instructionStringRemoveNCharFail" ["this is a sentence i"] [GeneChar 'z', GeneString "this is a sentence i", GeneInt 2, StateFunc instructionStringRemoveNChar] emptyState
  stringTestFunc "instructionStringRemoveAllCharSuccess" ["ths s a sentence "] [GeneChar 'i', GeneString "this is a sentence i", StateFunc instructionStringRemoveAllChar] emptyState
  stringTestFunc "instructionStringRemoveAllCharFail" ["this is a sentence i"] [GeneChar 'z', GeneString "this is a sentence i", StateFunc instructionStringRemoveAllChar] emptyState
  intTestFunc "instructionStringOccurrencesOfChar3" [3] [GeneChar 'i', GeneString "this is a sentence i", StateFunc instructionStringOccurrencesOfChar] emptyState
  intTestFunc "instructionStringOccurrencesOfChar0" [0] [GeneChar 'z', GeneString "this is a sentence i", StateFunc instructionStringOccurrencesOfChar] emptyState
  stringTestFunc "instructionStringReverse" ["321cba"] [GeneString "abc123", StateFunc instructionStringReverse] emptyState
  stringTestFunc "instructionStringHead3" ["abc"] [GeneString "abc123", GeneInt 3, StateFunc instructionStringHead] emptyState
  stringTestFunc "instructionStringHead0" [""] [GeneString "abc123", GeneInt 0, StateFunc instructionStringHead] emptyState
  stringTestFunc "instructionStringTail3" ["123"] [GeneString "abc123", GeneInt 3, StateFunc instructionStringTail] emptyState
  stringTestFunc "instructionStringTail0" [""] [GeneString "abc123", GeneInt 0, StateFunc instructionStringTail] emptyState
  stringTestFunc "instructionStringAppendChar" ["Rabc123"] [GeneString "abc123", GeneChar 'R', StateFunc instructionStringAppendChar] emptyState
  stringTestFunc "instructionStringRestFull" ["bc123"] [GeneString "abc123", StateFunc instructionStringRest] emptyState
  stringTestFunc "instructionStringRestEmpty" [""] [GeneString "", StateFunc instructionStringRest] emptyState
  stringTestFunc "instructionStringButLastFull" ["abc12"] [GeneString "abc123", StateFunc instructionStringButLast] emptyState
  stringTestFunc "instructionStringButLastEmpty" [""] [GeneString "", StateFunc instructionStringButLast] emptyState
  stringTestFunc "instructionStringDrop3" ["123"] [GeneString "abc123", GeneInt 3, StateFunc instructionStringDrop] emptyState
  stringTestFunc "instructionStringDrop0" ["abc123"] [GeneString "abc123", GeneInt 0, StateFunc instructionStringDrop] emptyState
  stringTestFunc "instructionStringButLastN3" ["abc"] [GeneString "abc123", GeneInt 3, StateFunc instructionStringButLastN] emptyState
  stringTestFunc "instructionStringButLastN0" ["abc123"] [GeneString "abc123", GeneInt 0, StateFunc instructionStringButLastN] emptyState
  intTestFunc "instructionStringLength6" [6] [GeneString "abc123", StateFunc instructionStringLength] emptyState
  stringTestFunc "instructionStringMakeEmpty" ["", "abc123"] [GeneString "abc123", StateFunc instructionStringMakeEmpty] emptyState
  stringTestFunc "instructionStringRemoveNth" ["abc23"] [GeneString "abc123", GeneInt 3, StateFunc instructionStringRemoveNth] emptyState
  stringTestFunc "instructionStringSetNth" ["abR123"] [GeneString "abc123", GeneInt 2, GeneChar 'R', StateFunc instructionStringSetNth] emptyState
  stringTestFunc "instructionStringStripWhitespace" ["abc123"] [GeneString " \r \n abc123 \t", StateFunc instructionStringStripWhitespace] emptyState
  stringTestFunc "instructionStringFromBoolTrue" ["True"] [GeneBool True, StateFunc instructionStringFromBool] emptyState
  stringTestFunc "instructionStringFromBoolTrue" ["False"] [GeneBool False, StateFunc instructionStringFromBool] emptyState
  stringTestFunc "instructionStringFromInt1000" ["1000"] [GeneInt 1000, StateFunc instructionStringFromInt] emptyState
  stringTestFunc "instructionStringFromInt-1" ["-1"] [GeneInt (-1), StateFunc instructionStringFromInt] emptyState
  stringTestFunc "instructionStringFromFloat3.2" ["3.2"] [GeneFloat 3.2, StateFunc instructionStringFromFloat] emptyState
  stringTestFunc "instructionStringFromFloat-99.0" ["-99.0"] [GeneFloat (-99.0), StateFunc instructionStringFromFloat] emptyState
  stringTestFunc "instructionStringFromChar" ["Z"] [GeneChar 'Z', StateFunc instructionStringFromChar] emptyState
  stringTestFunc "instructionStringFromChar" [" "] [GeneChar ' ', StateFunc instructionStringFromChar] emptyState

  -- char instructions
  stringTestFunc "instructionCharConcat" ["ab"] [GeneChar 'b', GeneChar 'a', StateFunc instructionCharConcat] emptyState
  charTestFunc "instructionCharFromFirstCharSuccess" ['a'] [GeneString "abc123", StateFunc instructionCharFromFirstChar] emptyState
  charTestFunc "instructionCharFromFirstCharFail" [] [GeneString "", StateFunc instructionCharFromFirstChar] emptyState
  charTestFunc "instructionCharFromLastCharSuccess" ['3'] [GeneString "abc123", StateFunc instructionCharFromLastChar] emptyState
  charTestFunc "instructionCharFromLastCharFail" [] [GeneString "", StateFunc instructionCharFromLastChar] emptyState
  charTestFunc "instructionCharFromNthCharSuccess" ['c'] [GeneString "abc123", GeneInt 2, StateFunc instructionCharFromNthChar] emptyState
  boolTestFunc "instructionCharIsWhitespaceSpace" [True] [GeneChar ' ', StateFunc instructionCharIsWhitespace] emptyState
  boolTestFunc "instructionCharIsWhitespacet" [True] [GeneChar '\t', StateFunc instructionCharIsWhitespace] emptyState
  boolTestFunc "instructionCharIsWhitespacer" [True] [GeneChar '\r', StateFunc instructionCharIsWhitespace] emptyState
  boolTestFunc "instructionCharIsWhitespacen" [True] [GeneChar '\n', StateFunc instructionCharIsWhitespace] emptyState
  boolTestFunc "instructionCharIsWhitespaceFalse" [False] [GeneChar 'a', StateFunc instructionCharIsWhitespace] emptyState
  boolTestFunc "instructionCharIsLetterTrue" [True] [GeneChar 'a', StateFunc instructionCharIsLetter] emptyState
  boolTestFunc "instructionCharIsLetterFalse" [False] [GeneChar '1', StateFunc instructionCharIsLetter] emptyState
  boolTestFunc "instructionCharIsDigitTrue" [True] [GeneChar '1', StateFunc instructionCharIsDigit] emptyState
  boolTestFunc "instructionCharIsDigitFalse" [False] [GeneChar 'a', StateFunc instructionCharIsDigit] emptyState

  -- vector int instructions
  vectorIntTestFunc "instructionVectorIntConcat" [[4, 5, 6, 1, 2, 3]] [GeneVectorInt [1, 2, 3], GeneVectorInt [4, 5, 6], StateFunc instructionVectorIntConcat] emptyState
  vectorIntTestFunc "instructionVectorIntConj" [[99, 1, 2, 3]] [GeneVectorInt [1, 2, 3], GeneInt 99, StateFunc instructionVectorIntConj] emptyState
  vectorIntTestFunc "instructionIntTakeN" [[1, 2], [6, 7, 8]] [GeneVectorInt [6, 7, 8], GeneVectorInt [1, 2, 3], GeneInt 2, StateFunc instructionVectorIntTakeN] emptyState
  vectorIntTestFunc "instructionVectorIntSubVector" [[1, 2, 3]] [GeneVectorInt [0, 1, 2, 3, 4, 5], GeneInt 3, GeneInt 1, StateFunc instructionVectorIntSubVector] emptyState
  intTestFunc "instructionVectorIntFirst" [1] [GeneVectorInt [1,2,3,4,5], StateFunc instructionVectorIntFirst] emptyState
  intTestFunc "instructionVectorIntLast" [5] [GeneVectorInt [1,2,3,4,5], StateFunc instructionVectorIntLast] emptyState
  intTestFunc "instructionVectorIntNthInBounds" [2] [GeneVectorInt [1,2,3,4,5], GeneInt 1, StateFunc instructionVectorIntNth] emptyState
  intTestFunc "instructionVectorIntNthOverflow" [2] [GeneVectorInt [1,2,3,4,5], GeneInt 6, StateFunc instructionVectorIntNth] emptyState
  vectorIntTestFunc "instructionVectorIntRestFull" [[2,3,4,5]] [GeneVectorInt [1,2,3,4,5], StateFunc instructionVectorIntRest] emptyState
  vectorIntTestFunc "instructionVectorIntRestEmpty" [[]] [GeneVectorInt [], StateFunc instructionVectorIntRest] emptyState
  vectorIntTestFunc "instructionVectorIntButLastFull" [[1,2,3,4]] [GeneVectorInt [1,2,3,4,5], StateFunc instructionVectorIntButLast] emptyState
  vectorIntTestFunc "instructionVectorIntButLastEmpty" [[]] [GeneVectorInt [], StateFunc instructionVectorIntButLast] emptyState
  intTestFunc "instructionVectorIntLength3" [3] [GeneVectorInt [1,2,3], StateFunc instructionVectorIntLength] emptyState
  intTestFunc "instructionVectorIntLength0" [0] [GeneVectorInt [], StateFunc instructionVectorIntLength] emptyState
  vectorIntTestFunc "instructionVectorIntReverse" [[4,3,2,1]] [GeneVectorInt [1,2,3,4], StateFunc instructionVectorIntReverse] emptyState
  intTestFunc "instructionVectorIntPushAllFull" [1,2,3,4,99] [GeneVectorInt [1,2,3,4], GeneInt 99, StateFunc instructionVectorIntPushAll] emptyState
  intTestFunc "instructionVectorIntPushAllEmpty" [99] [GeneVectorInt [], GeneInt 99, StateFunc instructionVectorIntPushAll] emptyState
  vectorIntTestFunc "instructionVectorIntMakeEmpty" [[]] [StateFunc instructionVectorIntMakeEmpty] emptyState
  boolTestFunc "instructionVectorIntIsEmptyTrue" [True] [GeneVectorInt [], StateFunc instructionVectorIntIsEmpty] emptyState
  boolTestFunc "instructionVectorIntIsEmptyFalse" [False] [GeneVectorInt [1,2,3,4], StateFunc instructionVectorIntIsEmpty] emptyState
  intTestFunc "instructionVectorIntIndexOf1" [1] [GeneVectorInt [1,2,3,4,5], GeneInt 2, StateFunc instructionVectorIntIndexOf] emptyState
  intTestFunc "instructionVectorIntIndexOfFail" [-1] [GeneVectorInt [], GeneInt 2, StateFunc instructionVectorIntIndexOf] emptyState
  intTestFunc "instructionVectorIntOccurrencesOf2" [2] [GeneVectorInt [1,2,3,4,2,6,7], GeneInt 2, StateFunc instructionVectorIntOccurrencesOf] emptyState
  intTestFunc "instructionVectorIntOccurrencesOf0" [0] [GeneVectorInt [1,2,3,4,2,6,7], GeneInt 0, StateFunc instructionVectorIntOccurrencesOf] emptyState
  vectorIntTestFunc "instructionVectorIntSetNth3" [[0,1,2,99,4,5]] [GeneVectorInt [0,1,2,3,4,5], GeneInt 99, GeneInt 3, StateFunc instructionVectorIntSetNth] emptyState
  vectorIntTestFunc "instructionVectorIntSetNth9" [[0,1,2,99,4,5]] [GeneVectorInt [0,1,2,3,4,5], GeneInt 99, GeneInt 9, StateFunc instructionVectorIntSetNth] emptyState
  vectorIntTestFunc "instructionVectorIntReplace3" [[0,1,2,99,4,5,99,5,99]] [GeneInt 99, GeneInt 3, GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntReplace] emptyState
  vectorIntTestFunc "instructionVectorIntReplace-1" [[0,1,2,3,4,5,3,5,3]] [GeneInt 99, GeneInt (-1), GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntReplace] emptyState
  vectorIntTestFunc "instructionVectorIntReplaceFirst3" [[0,1,2,99,4,5,3,5,3]] [GeneInt 99, GeneInt 3, GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntReplaceFirst] emptyState
  vectorIntTestFunc "instructionVectorIntReplaceFirst-2" [[0,1,2,3,4,5,3,5,3]] [GeneInt 99, GeneInt (-2), GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntReplaceFirst] emptyState
  vectorIntTestFunc "instructionVectorIntRemove" [[0,1,2,4,5,5]] [GeneInt 3, GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntRemove] emptyState
  intTestFunc "instructionVectorIntIterate" [66] [GeneInt 40, GeneVectorInt [0,1,2,3,4,5,3,5,3], StateFunc instructionVectorIntIterate, StateFunc instructionIntAdd] emptyState
