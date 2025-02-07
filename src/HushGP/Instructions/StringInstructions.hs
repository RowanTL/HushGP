module HushGP.Instructions.StringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
import Data.List.Split
import Control.Lens

-- shamelessly stolen from https://hackage.haskell.org/package/MissingH-1.6.0.1/docs/src/Data.String.Utils.html#strip
wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if x `elem` wschars
                            then lstrip xs
                            else s

-- this is a tad inefficient init
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

instructionStringConcat :: State -> State
instructionStringConcat state = instructionConcat state string

instructionStringSwap :: State -> State
instructionStringSwap state = instructionSwap state string

instructionStringInsertString :: State -> State
instructionStringInsertString state@(State{_string = s1 : s2 : ss, _int = i1 : is}) = state {_string = combineTupleList s2 (splitAt i1 s1) : ss, _int = is}
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
instructionStringInsertChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state{_string = combineTuple c1 (splitAt i1 s1) : ss, _char = cs, _int = is}
instructionStringInsertChar state = state

instructionStringContainsChar :: State -> State
instructionStringContainsChar state = instructionVectorContains state char string

instructionStringIndexOfChar :: State -> State
instructionStringIndexOfChar state = instructionVectorIndexOf state char string

instructionStringSplitOnChar :: State -> State
instructionStringSplitOnChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state {_string = reverse $ splitOn [c1] s1 <> ss, _char = cs}
instructionStringSplitOnChar state = state

instructionStringReplaceFirstChar :: State -> State
instructionStringReplaceFirstChar state = instructionVectorReplaceFirst state char string

instructionStringReplaceNChar :: State -> State
instructionStringReplaceNChar state@(State {_string = s1 : ss, _char = c1 : c2 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] [c2] (Just i1) : ss, _char = cs, _int = is}
instructionStringReplaceNChar state = state

instructionStringReplaceAllChar :: State -> State
instructionStringReplaceAllChar state = instructionVectorReplace state char string

instructionStringRemoveFirstChar :: State -> State
instructionStringRemoveFirstChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state {_string = replace s1 [c1] "" (Just 1) : ss, _char = cs}
instructionStringRemoveFirstChar state = state

instructionStringRemoveNChar :: State -> State
instructionStringRemoveNChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] "" (Just i1) : ss, _char = cs, _int = is}
instructionStringRemoveNChar state = state

instructionStringRemoveAllChar :: State -> State
instructionStringRemoveAllChar state = instructionVectorRemove state char string

instructionStringOccurrencesOfChar :: State -> State
instructionStringOccurrencesOfChar state = instructionVectorOccurrencesOf state char string

instructionStringReverse :: State -> State
instructionStringReverse state = instructionReverse state string

instructionStringHead :: State -> State
instructionStringHead state = instructionTakeN state string

instructionStringTail :: State -> State
instructionStringTail state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = takeR (absNum i1 s1) s1 : ss, _int = is}
instructionStringTail state = state

instructionStringAppendChar :: State -> State
instructionStringAppendChar state = instructionConj state char string

instructionStringConjEndChar :: State -> State
instructionStringConjEndChar = instructionConjEnd char string

instructionStringRest :: State -> State
instructionStringRest state = instructionRest state string

instructionStringButLast :: State -> State
instructionStringButLast state = instructionButLast state string

instructionStringDrop :: State -> State
instructionStringDrop state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = drop (absNum i1 s1) s1 : ss, _int = is}
instructionStringDrop state = state

instructionStringButLastN :: State -> State
instructionStringButLastN state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = dropR (absNum i1 s1) s1 : ss, _int = is}
instructionStringButLastN state = state

instructionStringLength :: State -> State
instructionStringLength state = instructionLength state string

instructionStringMakeEmpty :: State -> State
instructionStringMakeEmpty state = instructionVectorMakeEmpty state string

instructionStringIsEmptyString :: State -> State
instructionStringIsEmptyString state@(State {_string = s1 : ss, _bool = bs}) = state{_string = ss, _bool = null s1 : bs}
instructionStringIsEmptyString state = state

instructionStringRemoveNth :: State -> State
instructionStringRemoveNth state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = deleteAt (absNum i1 s1) s1 : ss, _int = is}
instructionStringRemoveNth state = state

instructionStringSetNth :: State -> State
instructionStringSetNth state = instructionVectorSetNth state char string

instructionStringStripWhitespace :: State -> State
instructionStringStripWhitespace state@(State {_string = s1 : ss}) = state{_string = strip s1 : ss}
instructionStringStripWhitespace state = state

instructionStringFromLens :: Show a => State -> Lens' State [a] -> State
instructionStringFromLens state@(State {_string = ss}) accessor =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x,_) -> state{_string = show x : ss}

instructionStringFromBool :: State -> State
instructionStringFromBool state = instructionStringFromLens state bool

instructionStringFromInt :: State -> State
instructionStringFromInt state = instructionStringFromLens state int

instructionStringFromFloat :: State -> State
instructionStringFromFloat state = instructionStringFromLens state float

instructionStringFromChar :: State -> State
instructionStringFromChar state@(State {_string = ss, _char = c1 : cs}) = state{_string = [c1] : ss, _char = cs}
instructionStringFromChar state = state

instructionStringPop :: State -> State
instructionStringPop state = instructionPop state string

instructionStringDup :: State -> State
instructionStringDup state = instructionDup state string

instructionStringDupN :: State -> State
instructionStringDupN state = instructionDupN state string

instructionStringRot :: State -> State
instructionStringRot state = instructionRot state string

instructionStringFlush :: State -> State
instructionStringFlush state = instructionFlush state string

instructionStringEq :: State -> State
instructionStringEq state = instructionEq state string

instructionStringStackDepth :: State -> State
instructionStringStackDepth state = instructionStackDepth state string

instructionStringYank :: State -> State
instructionStringYank state = instructionYank state string

instructionStringYankDup :: State -> State
instructionStringYankDup state = instructionYankDup state string

instructionStringIsEmpty :: State -> State
instructionStringIsEmpty state = instructionIsEmpty state string

instructionStringShove :: State -> State
instructionStringShove state = instructionShove state string

instructionStringShoveDup :: State -> State
instructionStringShoveDup state = instructionShoveDup state string

instructionStringSort :: State -> State
instructionStringSort = instructionVectorSort string

instructionStringSortReverse :: State -> State
instructionStringSortReverse = instructionVectorSortReverse string

instructionStringDupItems :: State -> State
instructionStringDupItems = instructionDupItems string

instructionStringParseToChar :: State -> State
instructionStringParseToChar = instructionVectorParseToPrim string
