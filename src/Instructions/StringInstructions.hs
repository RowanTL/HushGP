module Instructions.StringInstructions where

import State
import Instructions.GenericInstructions
import Data.List.Split

absNum :: Integral a => a -> [b] -> Int
absNum rawNum lst = abs (fromIntegral rawNum) `mod` length lst

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
instructionStringFromNthChar state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = [s1 !! absNum i1 s1] : ss, _int = is}
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
instructionStringInsertChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state{_string = combineString [c1] (splitAt i1 s1) : ss, _char = cs, _int = is}
instructionStringInsertChar state = state

instructionStringContainsChar :: State -> State
instructionStringContainsChar state@(State {_string = s1 : ss, _char = c1 : cs, _bool = bs}) = state{_string = ss, _char = cs, _bool = (findSubA s1 [c1] /= -1) : bs}
instructionStringContainsChar state = state

instructionStringIndexOfChar :: State -> State
instructionStringIndexOfChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = is}) = state{_string = ss, _char = cs, _int = findSubA s1 [c1] : is}
instructionStringIndexOfChar state = state

instructionStringSplitOnChar :: State -> State
instructionStringSplitOnChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state {_string = reverse $ splitOn [c1] s1 <> ss, _char = cs}
instructionStringSplitOnChar state = state

instructionStringReplaceFirstChar :: State -> State
instructionStringReplaceFirstChar state@(State {_string = s1 : ss, _char = c1 : c2 : cs}) = state {_string = replace s1 [c1] [c2] (Just 1) : ss, _char = cs}
instructionStringReplaceFirstChar state = state

instructionStringReplaceNChar :: State -> State
instructionStringReplaceNChar state@(State {_string = s1 : ss, _char = c1 : c2 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] [c2] (Just i1) : ss, _char = cs, _int = is}
instructionStringReplaceNChar state = state

instructionStringReplaceAllChar :: State -> State
instructionStringReplaceAllChar state@(State {_string = s1 : ss, _char = c1 : c2 : cs}) = state{_string = replace s1 [c1] [c2] Nothing : ss, _char = cs}
instructionStringReplaceAllChar state = state

instructionStringRemoveFirstChar :: State -> State
instructionStringRemoveFirstChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state {_string = replace s1 [c1] "" (Just 1) : ss, _char = cs}
instructionStringRemoveFirstChar state = state

instructionStringRemoveNChar :: State -> State
instructionStringRemoveNChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] "" (Just i1) : ss, _char = cs, _int = is}
instructionStringRemoveNChar state = state

instructionStringRemoveAllChar :: State -> State
instructionStringRemoveAllChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state{_string = replace s1 [c1] "" Nothing : ss, _char = cs}
instructionStringRemoveAllChar state = state

instructionStringOccurrencesOfChar :: State -> State
instructionStringOccurrencesOfChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = is}) = state{_string = ss, _char = cs, _int = amtOccurences s1 [c1] : is}
instructionStringOccurrencesOfChar state = state

instructionStringReverse :: State -> State
instructionStringReverse state@(State {_string = s1 : ss}) = state{_string = reverse s1 : ss}
instructionStringReverse state = state

instructionStringHead :: State -> State
instructionStringHead state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = take (absNum i1 s1) s1 : ss, _int = is}
instructionStringHead state = state

instructionStringTail :: State -> State
instructionStringTail state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = takeR (absNum i1 s1) s1 : ss, _int = is}
instructionStringTail state = state

instructionStringAppendChar :: State -> State
instructionStringAppendChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state{_string = (c1 : s1) : ss, _char = cs}
instructionStringAppendChar state = state

instructionStringRest :: State -> State
instructionStringRest state@(State {_string = s1 : ss}) = state{_string = drop 1 s1 : ss}
instructionStringRest state = state

instructionStringButLast :: State -> State
instructionStringButLast state@(State {_string = s1 : ss}) =
  if not $ null s1
    then state{_string = init s1 : ss}
    else state
instructionStringButLast state = state

instructionStringDrop :: State -> State
instructionStringDrop state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = drop (absNum i1 s1) s1 : ss, _int = is}
instructionStringDrop state = state

instructionStringButLastN :: State -> State
instructionStringButLastN state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = dropR (absNum i1 s1) s1 : ss, _int = is}
instructionStringButLastN state = state

instructionStringLength :: State -> State
instructionStringLength state@(State {_string = s1 : ss, _int = is}) = state{_string = ss, _int = length s1 : is}
instructionStringLength state = state

instructionStringMakeEmpty :: State -> State
instructionStringMakeEmpty state@(State {_string = ss}) = state{_string = "" : ss}

instructionStringIsEmptyString :: State -> State
instructionStringIsEmptyString state@(State {_string = s1 : ss, _bool = bs}) = state{_string = ss, _bool = null s1 : bs}
instructionStringIsEmptyString state = state

instructionStringRemoveNth :: State -> State
instructionStringRemoveNth state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = deleteAt (absNum i1 s1) s1 : ss, _int = is}
instructionStringRemoveNth state = state
