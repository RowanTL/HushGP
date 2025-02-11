module HushGP.Instructions.VectorStringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

-- |Pops the top string vector from the string vector stack.
instructionVectorStringPop :: State -> State
instructionVectorStringPop = instructionPop vectorString

-- |Duplicates the top string vector from the string vector stack.
instructionVectorStringDup :: State -> State
instructionVectorStringDup = instructionDup vectorString

-- |Duplicates the top string vector from the string vector stack N times
-- based on the top int from the int stack.
instructionVectorStringDupN :: State -> State
instructionVectorStringDupN = instructionDupN vectorString

-- |Swaps the top two string vectors from the string vector stack.
instructionVectorStringSwap :: State -> State
instructionVectorStringSwap = instructionSwap vectorString

-- |Rotates the top three string vectors from the string vector stack.
instructionVectorStringRot :: State -> State
instructionVectorStringRot = instructionRot vectorString

-- |Sets the vector string stack to []
instructionVectorStringFlush :: State -> State
instructionVectorStringFlush = instructionFlush vectorString

-- |Pushes True to the bool stack if the top two string vectors from
-- the vector string stack are equal. Pushes False otherwise.
instructionVectorStringEq :: State -> State
instructionVectorStringEq = instructionEq vectorString

-- |Calculates the size of the vector string stack and pushes that number
-- to the int stack.
instructionVectorStringStackDepth :: State -> State
instructionVectorStringStackDepth = instructionStackDepth vectorString

-- |Moves an item from deep within the vector string stack to the top of the vector string stack based on
-- the top int from the int stack.
instructionVectorStringYank :: State -> State
instructionVectorStringYank = instructionYank vectorString

-- |Copies an item from deep within the vector string stack to the top of the vector string stack based on
-- the top int from the int stack.
instructionVectorStringYankDup :: State -> State
instructionVectorStringYankDup = instructionYankDup vectorString

-- |Pushes True to the bool stack if the vector string stack is empty. False if not.
instructionVectorStringIsStackEmpty :: State -> State
instructionVectorStringIsStackEmpty = instructionIsStackEmpty vectorString

-- |Moves an item from the top of the vector string stack to deep within the vector string stack based on
-- the top int from the int stack.
instructionVectorStringShove :: State -> State
instructionVectorStringShove = instructionShove vectorString

-- |Copies an item from the top of the vector string stack to deep within the vector string stack based on
-- the top int from the int stack.
instructionVectorStringShoveDup :: State -> State
instructionVectorStringShoveDup = instructionShoveDup vectorString

-- |Duplicate the top N items from the vector string stack based on the top int from the int stack.
instructionVectorStringDupItems :: State -> State
instructionVectorStringDupItems = instructionDupItems vectorString

-- |Concats the top two vectors on top of the vector string stack.
instructionVectorStringConcat :: State -> State
instructionVectorStringConcat = instructionVectorConcat vectorString

-- |Takes the top string from the string stack and prepends it to top string vector
-- on the string vector stack.
instructionVectorStringConj :: State -> State
instructionVectorStringConj = instructionVectorConj string vectorString

-- |Takes the top string from the string stack and appends it to top string vector
-- on the string vector stack.
instructionVectorStringConjEnd :: State -> State
instructionVectorStringConjEnd = instructionVectorConjEnd string vectorString

-- |Takes the first N strings from the top of the string vector from the string vector
-- and pushes the result to the string vector stack. N is pulled from the top of
-- the int stack.
instructionVectorStringTakeN :: State -> State
instructionVectorStringTakeN = instructionVectorTakeN vectorString

-- |Takes the last N strings from the top of the string vector from the string vector
-- and pushes the result to the string vector stack. N is pulled from the top of
-- the int stack.
instructionVectorStringTakeRN :: State -> State
instructionVectorStringTakeRN = instructionVectorTakeRN vectorString

-- |Takes a sublist of the top string vector on top of the vector string stack.
-- The two ints to determine bounds are pulled from the top of the int stack.
instructionVectorStringSubVector :: State -> State
instructionVectorStringSubVector = instructionSubVector vectorString

-- |Takes the first string from the top of the vector string stack and places
-- it on the string stack.
instructionVectorStringFirst :: State -> State
instructionVectorStringFirst = instructionVectorFirst string vectorString

-- |Takes the first string from the top of the vector string stack and places
-- it wrapped in a list on top of the vector string stack.
instructionVectorStringFromFirstPrim :: State -> State
instructionVectorStringFromFirstPrim = instructionVectorFromFirstPrim vectorString

-- |Takes the first string from the top of the string stack and places it
-- wrapped in a list on top of the vector string stack.
instructionVectorStringFromPrim :: State -> State
instructionVectorStringFromPrim = instructionVectorFromPrim string vectorString

-- |Takes the last string from the top of the vector string stack and places
-- it on the string stack.
instructionVectorStringLast :: State -> State
instructionVectorStringLast = instructionVectorLast string vectorString

-- |Takes the last string from the top string vector on the vector string stack and
-- places it on the string stack.
instructionVectorStringFromLastPrim :: State -> State
instructionVectorStringFromLastPrim = instructionVectorFromLastPrim vectorString

-- |Takes the Nth string from the top string vector and places it onto the string stack
-- based on an int from the top of the int stack.
instructionVectorStringNth :: State -> State
instructionVectorStringNth = instructionVectorNth string vectorString

-- |Takes the Nth string from the top string vector on the vector string stack and
-- creates a vector wrapping that Nth item, pushing it back onto the vector string stack.
-- N is the top item on the int stack.
instructionVectorStringFromNthPrim :: State -> State
instructionVectorStringFromNthPrim = instructionVectorFromNthPrim vectorString

-- |Removes the first string from the top string vector on the vector string stack and
-- places the result back onto the vector string stack.
instructionVectorStringRest :: State -> State
instructionVectorStringRest = instructionVectorRest vectorString

-- |Removes the last string from the top string vector on the vector string stack and
-- places the result back onto the vector string stack.
instructionVectorStringButLast :: State -> State
instructionVectorStringButLast = instructionVectorButLast vectorString

-- |Drops the first N items from the top string vector and pushes the result
-- back to the vector string stack. N is pulled from the top of the int stack.
instructionVectorStringDrop :: State -> State
instructionVectorStringDrop = instructionVectorDrop vectorString

-- |Drops the last N items from the top string vector and pushes the result
-- back to the vector string stack. N is pulled from the top of the int stack.
instructionVectorStringDropR :: State -> State
instructionVectorStringDropR = instructionVectorDropR vectorString

-- |Pushes the length of the top string vector from the vector string stack
-- to the top of the int stack.
instructionVectorStringLength :: State -> State
instructionVectorStringLength = instructionLength vectorString

-- |Reverses the top string vector from the vector string stack and pushes the
-- result to the vector string stack.
instructionVectorStringReverse :: State -> State
instructionVectorStringReverse = instructionReverse vectorString

-- |Takes the top string vector from the vector string stack and pushes the
-- individual strings to the vector string stack.
instructionVectorStringPushAll :: State -> State
instructionVectorStringPushAll = instructionPushAll string vectorString

-- |Makes an empty vector and pushes it to the vector string stack.
instructionVectorStringMakeEmpty :: State -> State
instructionVectorStringMakeEmpty = instructionVectorMakeEmpty vectorString

-- |Checks if the top string vector from the vector string stack is empty.
-- Pushes True if the string vector is empty to the bool stack. False otherwise.
instructionVectorStringIsEmpty :: State -> State
instructionVectorStringIsEmpty = instructionVectorIsEmpty vectorString

-- |If the top string vector from the vector string stack contains the top string from the string
-- stack, pushes True to the bool stack and pushes False otherwise.
instructionVectorStringContains :: State -> State
instructionVectorStringContains = instructionVectorContains string vectorString

-- |If the second to top string vector can be found within the first string vector from the
-- vector string stack, pushes True to the bool stack if is found, else False.
instructionVectorStringContainsVectorString :: State -> State
instructionVectorStringContainsVectorString = instructionVectorContainsVector vectorString

-- |Finds the first index of the top string in the string stack inside of the
-- top string vector from the vector string stack and pushes the result to the int stack.
instructionVectorStringIndexOf :: State -> State
instructionVectorStringIndexOf = instructionVectorIndexOf string vectorString

-- |Searches and pushes the index of the second string vector inside of the first
-- string vector to the int stack from the vector string stack. Pushes -1 if not found.
instructionVectorStringIndexOfVectorString :: State -> State
instructionVectorStringIndexOfVectorString = instructionVectorIndexOfVector vectorString

-- |Finds the amount of times the top string on the string stack occurs inside of
-- the top string vector from the vector string stack and pushes the result to the
-- int stack.
instructionVectorStringOccurrencesOf :: State -> State
instructionVectorStringOccurrencesOf = instructionVectorOccurrencesOf string vectorString

-- |Counts the amount of occurrences of the second string vector within the first
-- string vector. Pushes the result to the int stack.
instructionVectorStringOccurrencesOfVectorString :: State -> State
instructionVectorStringOccurrencesOfVectorString = instructionVectorOccurrencesOfVector vectorString

-- |Splits the top string vector from the vector string stack into lists of size one and pushes
-- the result back one the vector string stack.
instructionVectorStringParseToString :: State -> State
instructionVectorStringParseToString = instructionVectorParseToPrim vectorString

-- |Sets the Nth index inside of the top string vector from the vector string stack to the
-- top value from the primitive stack. N is pulled from the top of the int stack.
instructionVectorStringSetNth :: State -> State
instructionVectorStringSetNth = instructionVectorSetNth string vectorString

-- |Splits the string vector on top of the vector string stack with the string from the top
-- of the string stack and pushes the result to the original vector stack.
instructionVectorStringSplitOn :: State -> State
instructionVectorStringSplitOn = instructionVectorSplitOn string vectorString

-- |Splits the first string vector based on the second string vector from the vector
-- string stack and pushes the result to the vector string stack.
instructionVectorStringSplitOnVectorString :: State -> State
instructionVectorStringSplitOnVectorString = instructionVectorSplitOnVector vectorString

-- |Replaces the first occurrence of the top string with the second string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack.
instructionVectorStringReplaceFirst :: State -> State
instructionVectorStringReplaceFirst = instructionVectorReplace string vectorString (Just 1)

-- |Replaces all occurrences of the top string with the second string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack.
instructionVectorStringReplaceAll :: State -> State
instructionVectorStringReplaceAll = instructionVectorReplace string vectorString Nothing

-- |Replaces N occurrences of the top string with the second string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack. N is pulled from
-- the top of the int stack.
instructionVectorStringReplaceN :: State -> State
instructionVectorStringReplaceN = instructionVectorReplaceN string vectorString

-- |Replaces the first occurrence of the second string vector with the third string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack.
instructionVectorStringReplaceFirstVectorString :: State -> State
instructionVectorStringReplaceFirstVectorString = instructionVectorReplaceVector vectorString (Just 1)

-- |Replaces all occurrences of the second string vector with the third string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack.
instructionVectorStringReplaceAllVectorString :: State -> State
instructionVectorStringReplaceAllVectorString = instructionVectorReplaceVector vectorString Nothing

-- |Replaces N occurrences of the second string vector with the third string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack. N is pulled from the top of the int stack.
instructionVectorStringReplaceVectorStringN :: State -> State
instructionVectorStringReplaceVectorStringN = instructionVectorReplaceVectorN vectorString

-- |Removes the first occurrence of the top string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack.
instructionVectorStringRemoveFirst :: State -> State
instructionVectorStringRemoveFirst = instructionVectorRemove string vectorString (Just 1)

-- |Removes the all occurrences of the top string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack.
instructionVectorStringRemoveAll :: State -> State
instructionVectorStringRemoveAll = instructionVectorRemove string vectorString Nothing

-- |Removes N occurrences of the top string from
-- the string stack inside of the top string vector from the vector string stack.
-- Pushes the modified string vector to the vector string stack. N is pulled
-- from the top of the int stack.
instructionVectorStringRemoveN :: State -> State
instructionVectorStringRemoveN = instructionVectorRemoveN string vectorString

-- |Removes the first occurrence of the second string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack.
instructionVectorStringRemoveFirstVectorString :: State -> State
instructionVectorStringRemoveFirstVectorString = instructionVectorRemoveVector vectorString (Just 1)

-- |Removes all occurrences of the second string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack.
instructionVectorStringRemoveAllVectorString :: State -> State
instructionVectorStringRemoveAllVectorString = instructionVectorRemoveVector vectorString Nothing

-- |Removes N occurrences of the second string vector
-- inside of the first string vector from the vector string stack. Pushes the result to the
-- vector string stack. N is pulled from the top of the int stack.
instructionVectorStringRemoveNVectorString :: State -> State
instructionVectorStringRemoveNVectorString = instructionVectorRemoveVectorN vectorString

-- |Iterates over the top string vector on the vector string stack, applying the top instruction of the
-- exec stack along the way.
instructionVectorStringIterate :: State -> State
instructionVectorStringIterate = instructionVectorIterate string vectorString GeneVectorString instructionVectorStringIterate "instructionVectorStringIterate"

-- |Sorts the top string vector on the vector string stack and pushes the result back to the
-- vector string stack.
instructionVectorStringSort :: State -> State
instructionVectorStringSort = instructionVectorSort vectorString

-- |Sorts the top string vector on the vector string stack, reverses it, and pushes the result back to the
-- vector string stack.
instructionVectorStringSortReverse :: State -> State
instructionVectorStringSortReverse = instructionVectorSortReverse vectorString

-- |Inserts the top string from the string stack into the top string vector from the
-- vector string stack at a specified index and pushes the result to the vector
-- string stack. The index is pulled from the top of the int stack.
instructionVectorStringInsert :: State -> State
instructionVectorStringInsert = instructionVectorInsert string vectorString

-- |Inserts the second string vector into the first string vector from the vector string stack
-- at a specified index and pushes the result to the vector string stack. The index is
-- pulled from the top of the int stack.
instructionVectorStringInsertVectorString :: State -> State
instructionVectorStringInsertVectorString = instructionVectorInsertVector vectorString
