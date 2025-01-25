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

instructionStringIndexOfString :: State -> State
instructionStringIndexOfString state@(State {_string = s1 : s2 : ss, _int = is}) = state {_string = ss, _int = findSubA s1 s2 : is}
instructionStringIndexOfString state = state

instructionStringContainsString :: State -> State
instructionStringContainsString state@(State {_string = s1 : s2 : ss, _bool = bs}) = state {_string = ss, _bool = (findSubA s1 s2 /= -1) : bs}
instructionStringContainsString state = state

-- pysh reverses this. Check this for propeller
instructionStringSplitOnString :: State -> State
instructionStringSplitOnString state@(State {_string = s1 : s2 : ss}) = state {_string = reverse $ splitOn s2 s1 <> ss}
instructionStringSplitOnString state = state

instructionStringReplaceFirstString :: State -> State
instructionStringReplaceFirstString state@(State {_string = s1 : s2 : s3 : ss}) = state {_string = replace s1 s2 s3 (Just 1) : ss}
instructionStringReplaceFirstString state = state

instructionStringReplaceNString :: State -> State
instructionStringReplaceNString state@(State {_string = s1 : s2 : s3 : ss, _int = i1 : is}) = state{_string = replace s1 s2 s3 (Just i1) : ss, _int = is}
instructionStringReplaceNString state = state

instructionStringReplaceAllString :: State -> State
instructionStringReplaceAllString state@(State {_string = s1 : s2 : s3 : ss}) = state{_string = replace s1 s2 s3 Nothing : ss}
instructionStringReplaceAllString state = state

instructionStringRemoveFirstString :: State -> State
instructionStringRemoveFirstString state@(State {_string = s1 : s2 : ss}) = state{_string = replace s1 s2 "" (Just 1) : ss}
instructionStringRemoveFirstString state = state

instructionStringRemoveNString :: State -> State
instructionStringRemoveNString state@(State {_string = s1 : s2 : ss, _int = i1 : is}) = state{_string = replace s1 s2 "" (Just i1) : ss, _int = is}
instructionStringRemoveNString state = state

instructionStringRemoveAllString :: State -> State
instructionStringRemoveAllString state@(State {_string = s1 : s2 : ss}) = state{_string = replace s1 s2 "" Nothing : ss}
instructionStringRemoveAllString state = state

instructionStringOccurrencesOfString :: State -> State
instructionStringOccurrencesOfString state@(State {_string = s1 : s2 : ss, _int = is}) = state{_string = ss, _int = amtOccurences s1 s2 : is}
instructionStringOccurrencesOfString state = state

instructionStringInsertChar :: State -> State
instructionStringInsertChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state {_string = combineString [c1] (splitAt i1 s1) : ss, _char = cs, _int = is}
instructionStringInsertChar state = state
