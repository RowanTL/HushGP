module HushGP.Instructions.StringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
import Data.List.Split
import Control.Lens

-- |Utility String: Whitespack characters.
-- shamelessly stolen from https://hackage.haskell.org/package/MissingH-1.6.0.1/docs/src/Data.String.Utils.html#strip
wschars :: String
wschars = " \t\r\n"

-- |Utility Function: Strips a string of its whitespace on both sides.
strip :: String -> String
strip = lstrip . rstrip

-- |Utility Function: Strips a string of its whitespace on the left side.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if x `elem` wschars
                            then lstrip xs
                            else s

-- |Utility Function: Strips a string of its whitespace on the right side.
-- this is a tad inefficient
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- |Concats the top two strings on the string stack and pushes the result.
instructionStringConcat :: State -> State
instructionStringConcat = instructionConcat string

-- |Swaps the top two strings on the string stack.
instructionStringSwap :: State -> State
instructionStringSwap = instructionSwap string

-- |Inserts the second string on the string stack into the first string
-- on the string stack based on an int from the int stack.
instructionStringInsertString :: State -> State
instructionStringInsertString = instructionVectorInsertVector string
-- instructionStringInsertString state@(State{_string = s1 : s2 : ss, _int = i1 : is}) = state {_string = combineTupleList s2 (splitAt i1 s1) : ss, _int = is}
-- instructionStringInsertString state = state

-- |Takes the first string from the string stack and pushes the first character
-- back to the string stack as a string.
instructionStringFromFirstChar :: State -> State
instructionStringFromFirstChar = instructionVectorFromFirstPrim string
-- instructionStringFromFirstChar state@(State {_string = (schar : _) : ss}) = state {_string = [schar] : ss}
-- instructionStringFromFirstChar state = state

-- |Takes the first string from the string stack and pushes the last character
-- back to the string stack as a string.
instructionStringFromLastChar :: State -> State
instructionStringFromLastChar = instructionVectorFromLastPrim string
-- instructionStringFromLastChar state@(State {_string = s1 : ss}) =
  -- if not $ null s1
    -- then state {_string = [last s1] : ss}
    -- else state
-- instructionStringFromLastChar state = state

-- |Takes the first string from the string stack and pushes the Nth character
-- back to the string stack as a string. N in is the top int of the int stack.
instructionStringFromNthChar :: State -> State
instructionStringFromNthChar = instructionVectorFromNthPrim string
-- instructionStringFromNthChar state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = [s1 !! absNum i1 s1] : ss, _int = is}
-- instructionStringFromNthChar state = state

-- |Takes the first two strings from the top of the string stack. Looks for and pushed the
-- index of the second substring inside of the first substring to the int stack.
-- If not found, returns -1.
instructionStringIndexOfString :: State -> State
instructionStringIndexOfString = instructionVectorIndexOfVector string
-- instructionStringIndexOfString state@(State {_string = s1 : s2 : ss, _int = is}) = state {_string = ss, _int = findSubA s1 s2 : is}
-- instructionStringIndexOfString state = state

-- |Takes the first two strings from the top of the string stack. Pushes True to the
-- bool stack if the second string is contained within the first string. Pushes False otherwise.
instructionStringContainsString :: State -> State
instructionStringContainsString = instructionVectorContainsVector string
-- instructionStringContainsString state@(State {_string = s1 : s2 : ss, _bool = bs}) = state {_string = ss, _bool = (findSubA s1 s2 /= -1) : bs}
-- instructionStringContainsString state = state

-- |Takes the first two strings from the top of the string stack. Splits the first string
-- based on the second string and pushes the result to the string stack.
-- pysh reverses this. Check this for propeller
instructionStringSplitOnString :: State -> State
instructionStringSplitOnString = instructionVectorSplitOnVector string
-- instructionStringSplitOnString state@(State {_string = s1 : s2 : ss}) = state {_string = reverse $ splitOn s2 s1 <> ss}
-- instructionStringSplitOnString state = state

-- |Takes the first three strings from the top of the string stack. Replaces the first instance of
-- the second string within the first string with the third string. Pushes the result to the string stack.
instructionStringReplaceFirstString :: State -> State
instructionStringReplaceFirstString = instructionVectorReplaceVector string (Just 1)

-- |Takes the first three strings from the top of the string stack. Replaces the number of instances based on the of the int stack of
-- the second string within the first string with the third string. Pushes the result to the string stack.
instructionStringReplaceNString :: State -> State
instructionStringReplaceNString = instructionVectorReplaceVectorN string

-- |Takes the first three strings from the top of the string stack. Replaces all instances of
-- the second string within the first string with the third string. Pushes the result to the string stack.
instructionStringReplaceAllString :: State -> State
instructionStringReplaceAllString = instructionVectorReplaceVector string Nothing

-- |Takes the first two strings from the top of the string stack. Removes the first instance of
-- the second string. Pushes the result to the string stack.
instructionStringRemoveFirstString :: State -> State
instructionStringRemoveFirstString = instructionVectorRemoveVector string (Just 1)

-- |Takes the first two strings from the top of the string stack. Removes N instances
-- based on the top int from the int stack of the second string. Pushes the result to the string stack.
instructionStringRemoveNString :: State -> State
instructionStringRemoveNString = instructionVectorRemoveVectorN string

-- |Takes the first two strings from the top of the string stack. Removes all instances of
-- the second string. Pushes the result to the string stack.
instructionStringRemoveAllString :: State -> State
instructionStringRemoveAllString = instructionVectorRemoveVector string Nothing

-- |Counts the amount of occurrences of the second string in the first
-- string. Pushes the result to the string stack.
instructionStringOccurrencesOfString :: State -> State
instructionStringOccurrencesOfString = instructionVectorOccurrencesOfVector string

-- |Inserts the top char of the char stack into the top string of the string
-- stack based on an index from the top int of the int stack.
instructionStringInsertChar :: State -> State
instructionStringInsertChar = instructionVectorInsert char string

-- |Pushes True to the bool stack if the top char on the char stack is within the
-- top string on the string stack. Pushes False otherwise.
instructionStringContainsChar :: State -> State
instructionStringContainsChar = instructionVectorContains char string

-- |Pushes the first index found of the top char of the char stack within the
-- first string in the string stack to the int stack.
instructionStringIndexOfChar :: State -> State
instructionStringIndexOfChar = instructionVectorIndexOf char string

-- |@TODO
instructionStringSplitOnChar :: State -> State
instructionStringSplitOnChar = instructionVectorSplitOn char string

instructionStringReplaceFirstChar :: State -> State
instructionStringReplaceFirstChar = instructionVectorReplace char string (Just 1)

instructionStringReplaceNChar :: State -> State
instructionStringReplaceNChar state@(State {_string = s1 : ss, _char = c1 : c2 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] [c2] (Just i1) : ss, _char = cs, _int = is}
instructionStringReplaceNChar state = state

instructionStringReplaceAllChar :: State -> State
instructionStringReplaceAllChar = instructionVectorReplace char string Nothing

instructionStringRemoveFirstChar :: State -> State
instructionStringRemoveFirstChar state@(State {_string = s1 : ss, _char = c1 : cs}) = state {_string = replace s1 [c1] "" (Just 1) : ss, _char = cs}
instructionStringRemoveFirstChar state = state

instructionStringRemoveNChar :: State -> State
instructionStringRemoveNChar state@(State {_string = s1 : ss, _char = c1 : cs, _int = i1 : is}) = state{_string = replace s1 [c1] "" (Just i1) : ss, _char = cs, _int = is}
instructionStringRemoveNChar state = state

instructionStringRemoveAllChar :: State -> State
instructionStringRemoveAllChar = instructionVectorRemove char string

instructionStringOccurrencesOfChar :: State -> State
instructionStringOccurrencesOfChar = instructionVectorOccurrencesOf char string

instructionStringReverse :: State -> State
instructionStringReverse = instructionReverse string

instructionStringHead :: State -> State
instructionStringHead = instructionTakeN string

instructionStringTail :: State -> State
instructionStringTail state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = takeR (absNum i1 s1) s1 : ss, _int = is}
instructionStringTail state = state

instructionStringAppendChar :: State -> State
instructionStringAppendChar = instructionConj char string

instructionStringConjEndChar :: State -> State
instructionStringConjEndChar = instructionConjEnd char string

instructionStringRest :: State -> State
instructionStringRest = instructionRest string

instructionStringButLast :: State -> State
instructionStringButLast = instructionButLast string

instructionStringDrop :: State -> State
instructionStringDrop state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = drop (absNum i1 s1) s1 : ss, _int = is}
instructionStringDrop state = state

instructionStringButLastN :: State -> State
instructionStringButLastN state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = dropR (absNum i1 s1) s1 : ss, _int = is}
instructionStringButLastN state = state

instructionStringLength :: State -> State
instructionStringLength = instructionLength string

instructionStringMakeEmpty :: State -> State
instructionStringMakeEmpty = instructionVectorMakeEmpty string

instructionStringIsEmptyString :: State -> State
instructionStringIsEmptyString state@(State {_string = s1 : ss, _bool = bs}) = state{_string = ss, _bool = null s1 : bs}
instructionStringIsEmptyString state = state

instructionStringRemoveNth :: State -> State
instructionStringRemoveNth state@(State {_string = s1 : ss, _int = i1 : is}) = state{_string = deleteAt (absNum i1 s1) s1 : ss, _int = is}
instructionStringRemoveNth state = state

instructionStringSetNth :: State -> State
instructionStringSetNth = instructionVectorSetNth char string

instructionStringStripWhitespace :: State -> State
instructionStringStripWhitespace state@(State {_string = s1 : ss}) = state{_string = strip s1 : ss}
instructionStringStripWhitespace state = state

instructionStringFromLens :: Show a => State -> Lens' State [a] -> State
instructionStringFromLens state@(State {_string = ss}) accessor =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x1,_) -> state{_string = show x1 : ss}

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
instructionStringPop = instructionPop string

instructionStringDup :: State -> State
instructionStringDup = instructionDup string

instructionStringDupN :: State -> State
instructionStringDupN = instructionDupN string

instructionStringRot :: State -> State
instructionStringRot = instructionRot string

instructionStringFlush :: State -> State
instructionStringFlush = instructionFlush string

instructionStringEq :: State -> State
instructionStringEq = instructionEq string

instructionStringStackDepth :: State -> State
instructionStringStackDepth = instructionStackDepth string

instructionStringYank :: State -> State
instructionStringYank = instructionYank string

instructionStringYankDup :: State -> State
instructionStringYankDup = instructionYankDup string

instructionStringIsStackEmpty :: State -> State
instructionStringIsStackEmpty = instructionIsStackEmpty string

instructionStringShove :: State -> State
instructionStringShove = instructionShove string

instructionStringShoveDup :: State -> State
instructionStringShoveDup = instructionShoveDup string

instructionStringSort :: State -> State
instructionStringSort = instructionVectorSort string

instructionStringSortReverse :: State -> State
instructionStringSortReverse = instructionVectorSortReverse string

instructionStringDupItems :: State -> State
instructionStringDupItems = instructionDupItems string

instructionStringParseToChar :: State -> State
instructionStringParseToChar = instructionVectorParseToPrim string

instructionStringSubString :: State -> State
instructionStringSubString = instructionSubVector string
