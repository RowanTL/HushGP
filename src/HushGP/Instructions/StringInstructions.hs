module HushGP.Instructions.StringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
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

-- |Takes the top string from the string stack and the top
-- char from the char stack. Splits the top string based on
-- the top char and pushes the result to the string stack.
instructionStringSplitOnChar :: State -> State
instructionStringSplitOnChar = instructionVectorSplitOn char string

-- |Takes the top string from the string stack and the two top char from the char stack.
-- Replaces the first instance of the top char with the second char.
instructionStringReplaceFirstChar :: State -> State
instructionStringReplaceFirstChar = instructionVectorReplace char string (Just 1)

-- |Takes the top string from the string stack and the two top char from the char stack.
-- Replaces N instances of the top char with the second char. N is determined by the
-- top int on the int stack.
instructionStringReplaceNChar :: State -> State
instructionStringReplaceNChar = instructionVectorReplaceN char string

-- |Takes the top string from the string stack and the two top char from the char stack.
-- Replaces all instances of the top char with the second char.
instructionStringReplaceAllChar :: State -> State
instructionStringReplaceAllChar = instructionVectorReplace char string Nothing

-- |Takes the top string from the string stack and the top char from the char stack.
-- Removes the first instance of the top char with the second char.
instructionStringRemoveFirstChar :: State -> State
instructionStringRemoveFirstChar = instructionVectorRemove char string (Just 1)

-- |Takes the top string from the string stack and the top char from the char stack.
-- Removes N instances of the top char with the second char. N is pulled from the top
-- of the int stack.
instructionStringRemoveNChar :: State -> State
instructionStringRemoveNChar = instructionVectorRemoveN char string

-- |Takes the top string from the string stack and the top char from the char stack.
-- Removes all instances of the top char with the second char.
instructionStringRemoveAllChar :: State -> State
instructionStringRemoveAllChar = instructionVectorRemove char string Nothing

-- |Takes the top string from the string stack and the top char from the char stack.
-- Counts the amount of occurrences of the top char inside of the top string. Pushes
-- this result to the int stack.
instructionStringOccurrencesOfChar :: State -> State
instructionStringOccurrencesOfChar = instructionVectorOccurrencesOf char string

-- |Takes the top string from the string stack and reverses it. Pushes the reversed string
-- to the top of the stack.
instructionStringReverse :: State -> State
instructionStringReverse = instructionReverse string

-- |Takes the top string from the string stack, takes the first N chars from the top string,
-- and pushes the result to the string stack. N is pulled from the top of the int stack.
instructionStringHead :: State -> State
instructionStringHead = instructionVectorTakeN string

-- |Takes the top string from the string stack, takes the last N chars from the top string,
-- and pushes the result to the string stack. N is pulled from the top of the int stack.
instructionStringTail :: State -> State
instructionStringTail = instructionVectorTakeRN string

-- |Takes the top string from the string stack and the top char from the char stack.
-- Prepends the top char to the top string. Pushes the result to the string stack.
instructionStringPrependChar :: State -> State
instructionStringPrependChar = instructionVectorConj char string

-- |Takes the top string from the string stack and the top char from the char stack.
-- Appends the top char to the top string. Pushes the result to the string stack.
instructionStringAppendChar :: State -> State
instructionStringAppendChar = instructionVectorConjEnd char string

-- |Takes the top string from the string stack and removes the first char
-- from said string. Pushes the result to the string stack.
instructionStringRest :: State -> State
instructionStringRest = instructionVectorRest string

-- |Takes the top string from the string stack and removes the last char
-- from said string. Pushes the result to the string stack.
instructionStringButLast :: State -> State
instructionStringButLast = instructionVectorButLast string

-- |Takes the top string from the string stack and drops the first N characters
-- from said string. Pushes the result to the string stack. N is pulled from the top
-- of the int stack.
instructionStringDrop :: State -> State
instructionStringDrop = instructionVectorDrop string

-- |Takes the top string from the string stack and drops the last N characters
-- from said string. Pushes the result to the string stack. N is pulled from the top
-- of the int stack.
instructionStringButLastN :: State -> State
instructionStringButLastN = instructionVectorDropR string

-- |Takes the top string from the string stack and calculates the length. The length
-- is then pushed to the int stack.
instructionStringLength :: State -> State
instructionStringLength = instructionLength string

-- |Makes an empty string and pushes it to the top of the string stack.
instructionStringMakeEmpty :: State -> State
instructionStringMakeEmpty = instructionVectorMakeEmpty string

-- |Checks to see if the top string is empty on the string stack.
-- Pushes True to the bool stack if empty. Pushes False if not.
instructionStringIsEmptyString :: State -> State
instructionStringIsEmptyString = instructionVectorIsEmpty string

-- TODO: Make this generic
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
