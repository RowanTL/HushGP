module Instructions.StringInstructions where

import State
import Instructions.GenericInstructions
import Data.List.Split

combineString :: String -> (String, String) -> String
combineString toInsert (front, back) = front <> toInsert <> back

instructionStringConcat :: State -> State
instructionStringConcat state = instructionConcat state string

instructionStringSwap :: State -> State
instructionStringSwap state = instructionSwap state string

instructionStringInsertString :: State -> State
instructionStringInsertString state@(State{_string = s1 : s2 : ss, _int = i1 : is}) = state {_string = combineString s2 (splitAt i1 s1) : ss, _int = is}
instructionStringInsertString state = state

instructionStringFromFirstChar :: State -> State
instructionStringFromFirstChar state@(State {_string = (schar : _) : ss}) = state {_string = [schar] : ss}
instructionStringFromFirstChar state = state

instructionStringFromLastChar :: State -> State
instructionStringFromLastChar state@(State {_string = s1 : ss}) =
  if not $ null s1
    then state {_string = [last s1] : ss}
    else state
instructionStringFromLastChar state = state

instructionStringFromNthChar :: State -> State
instructionStringFromNthChar state@(State {_string = s1 : ss, _int = i1 : is}) =
  let
    index = abs i1 `mod` length s1
  in
    state{_string = [s1 !! index] : ss, _int = is}
instructionStringFromNthChar state = state

-- instructionStringContainsString :: State -> State
-- instructionStringContainsString state@(State )

-- Haskell is kinda really cool. This can totally be
-- abstracted
findSubString :: String -> String -> Int
findSubString fullString subString 
  | length fullString < length subString = -1
  | length fullString == length subString = if fullString == subString then 0 else -1
  | otherwise = findSubString' fullString subString 0
  where
    findSubString' :: String -> String -> Int -> Int
    findSubString' fStr sStr index
      | null fStr = -1
      | length sStr > length fStr = -1
      | sStr == take (length sStr) fStr = index
      | otherwise = findSubString' (drop 1 fStr) sStr (index + 1)

instructionStringIndexOfString :: State -> State
instructionStringIndexOfString state@(State {_string = s1 : s2 : ss, _int = is}) = state {_string = ss, _int = findSubString s1 s2 : is}
instructionStringIndexOfString state = state

instructionStringContainsString :: State -> State
instructionStringContainsString state@(State {_string = s1 : s2 : ss, _bool = bs}) = state {_string = ss, _bool = (findSubString s1 s2 /= -1) : bs}
instructionStringContainsString state = state

instructionStringSplitOnString :: State -> State
instructionStringSplitOnString state@(State {_string = s1 : s2 : ss}) = state {_string = splitOn s2 s1 <> ss}
instructionStringSplitOnString state = state

instructionStringReplaceFirstString :: State -> State
instructionStringReplaceFirstString state@(State {_string = s1 : s2 : s3 : ss}) = undefined -- TODO
instructionStringReplaceFirstString state = state
