module HushGP.Instructions.VectorCharInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

-- |Pops the top char vector from the char vector stack.
instructionVectorCharPop :: State -> State
instructionVectorCharPop = instructionPop vectorChar

-- |Duplicates the top char vector from the char vector stack.
instructionVectorCharDup :: State -> State
instructionVectorCharDup = instructionDup vectorChar

-- |Duplicates the top char vector from the char vector stack N times
-- based on the top int from the int stack.
instructionVectorCharDupN :: State -> State
instructionVectorCharDupN = instructionDupN vectorChar

-- |Swaps the top two char vectors from the char vector stack.
instructionVectorCharSwap :: State -> State
instructionVectorCharSwap = instructionSwap vectorChar

-- |Rotates the top three char vectors from the char vector stack.
instructionVectorCharRot :: State -> State
instructionVectorCharRot = instructionRot vectorChar

-- |Sets the vector char stack to []
instructionVectorCharFlush :: State -> State
instructionVectorCharFlush = instructionFlush vectorChar

-- |Pushes True to the bool stack if the top two char vectors from
-- the vector char stack are equal. Pushes False otherwise.
instructionVectorCharEq :: State -> State
instructionVectorCharEq = instructionEq vectorChar

-- |Calculates the size of the vector char stack and pushes that number
-- to the int stack.
instructionVectorCharStackDepth :: State -> State
instructionVectorCharStackDepth = instructionStackDepth vectorChar

-- |Moves an item from deep within the vector char stack to the top of the vector char stack based on
-- the top int from the int stack.
instructionVectorCharYank :: State -> State
instructionVectorCharYank = instructionYank vectorChar

-- |Copies an item from deep within the vector char stack to the top of the vector char stack based on
-- the top int from the int stack.
instructionVectorCharYankDup :: State -> State
instructionVectorCharYankDup = instructionYankDup vectorChar

-- |Pushes True to the bool stack if the vector char stack is empty. False if not.
instructionVectorCharIsStackEmpty :: State -> State
instructionVectorCharIsStackEmpty = instructionIsStackEmpty vectorChar

-- |Moves an item from the top of the vector char stack to deep within the vector char stack based on
-- the top int from the int stack.
instructionVectorCharShove :: State -> State
instructionVectorCharShove = instructionShove vectorChar

-- |Copies an item from the top of the vector char stack to deep within the vector char stack based on
-- the top int from the int stack.
instructionVectorCharShoveDup :: State -> State
instructionVectorCharShoveDup = instructionShoveDup vectorChar

-- |Duplicate the top N items from the vector char stack based on the top int from the int stack.
instructionVectorCharDupItems :: State -> State
instructionVectorCharDupItems = instructionDupItems vectorChar

-- |Concats the top two vectors on top of the vector char stack.
instructionVectorCharConcat :: State -> State
instructionVectorCharConcat = instructionVectorConcat vectorChar

-- |Takes the top char from the char stack and prepends it to top char vector
-- on the char vector stack.
instructionVectorCharConj :: State -> State
instructionVectorCharConj = instructionVectorConj char vectorChar

-- |Takes the top char from the char stack and appends it to top char vector
-- on the char vector stack.
instructionVectorCharConjEnd :: State -> State
instructionVectorCharConjEnd = instructionVectorConjEnd char vectorChar

-- |Takes the first N chars from the top of the char vector from the char vector
-- and pushes the result to the char vector stack. N is pulled from the top of
-- the int stack.
instructionVectorCharTakeN :: State -> State
instructionVectorCharTakeN = instructionVectorTakeN vectorChar

-- |Takes the last N chars from the top of the char vector from the char vector
-- and pushes the result to the char vector stack. N is pulled from the top of
-- the int stack.
instructionVectorCharTakeRN :: State -> State
instructionVectorCharTakeRN = instructionVectorTakeRN vectorChar

-- |Takes a sublist of the top char vector on top of the vector char stack.
-- The two ints to determine bounds are pulled from the top of the int stack.
instructionVectorCharSubVector :: State -> State
instructionVectorCharSubVector = instructionSubVector vectorChar

-- |Takes the first char from the top of the vector char stack and places
-- it on the char stack.
instructionVectorCharFirst :: State -> State
instructionVectorCharFirst = instructionVectorFirst char vectorChar

-- |Takes the first char from the top of the vector char stack and places
-- it wrapped in a list on top of the vector char stack.
instructionVectorCharFromFirstPrim :: State -> State
instructionVectorCharFromFirstPrim = instructionVectorFromFirstPrim vectorChar

-- |Takes the first char from the top of the char stack and places it
-- wrapped in a list on top of the vector char stack.
instructionVectorCharFromPrim :: State -> State
instructionVectorCharFromPrim = instructionVectorFromPrim char vectorChar

-- |Takes the last char from the top of the vector char stack and places
-- it on the char stack.
instructionVectorCharLast :: State -> State
instructionVectorCharLast = instructionVectorLast char vectorChar

-- |Takes the last char from the top char vector on the vector char stack and
-- places it on the char stack.
instructionVectorCharFromLastPrim :: State -> State
instructionVectorCharFromLastPrim = instructionVectorFromLastPrim vectorChar

-- |Takes the Nth char from the top char vector and places it onto the char stack
-- based on an int from the top of the int stack.
instructionVectorCharNth :: State -> State
instructionVectorCharNth = instructionVectorNth char vectorChar

-- |Takes the Nth char from the top char vector on the vector char stack and
-- creates a vector wrapping that Nth item, pushing it back onto the vector char stack.
-- N is the top item on the int stack.
instructionVectorCharFromNthPrim :: State -> State
instructionVectorCharFromNthPrim = instructionVectorFromNthPrim vectorChar

-- |Removes the first char from the top char vector on the vector char stack and
-- places the result back onto the vector char stack.
instructionVectorCharRest :: State -> State
instructionVectorCharRest = instructionVectorRest vectorChar

-- |Removes the last char from the top char vector on the vector char stack and
-- places the result back onto the vector char stack.
instructionVectorCharButLast :: State -> State
instructionVectorCharButLast = instructionVectorButLast vectorChar

-- |Drops the first N items from the top char vector and pushes the result
-- back to the vector char stack. N is pulled from the top of the int stack.
instructionVectorCharDrop :: State -> State
instructionVectorCharDrop = instructionVectorDrop vectorChar

-- |Drops the last N items from the top char vector and pushes the result
-- back to the vector char stack. N is pulled from the top of the int stack.
instructionVectorCharDropR :: State -> State
instructionVectorCharDropR = instructionVectorDropR vectorChar

-- |Pushes the length of the top char vector from the vector char stack
-- to the top of the int stack.
instructionVectorCharLength :: State -> State
instructionVectorCharLength = instructionLength vectorChar

-- |Reverses the top char vector from the vector char stack and pushes the
-- result to the vector char stack.
instructionVectorCharReverse :: State -> State
instructionVectorCharReverse = instructionReverse vectorChar

-- |Takes the top char vector from the vector char stack and pushes the
-- individual chars to the vector char stack.
instructionVectorCharPushAll :: State -> State
instructionVectorCharPushAll = instructionPushAll char vectorChar

-- |Makes an empty vector and pushes it to the vector char stack.
instructionVectorCharMakeEmpty :: State -> State
instructionVectorCharMakeEmpty = instructionVectorMakeEmpty vectorChar

-- |Checks if the top char vector from the vector char stack is empty.
-- Pushes True if the char vector is empty to the bool stack. False otherwise.
instructionVectorCharIsEmpty :: State -> State
instructionVectorCharIsEmpty = instructionVectorIsEmpty vectorChar

-- |If the top char vector from the vector char stack contains the top char from the char
-- stack, pushes True to the bool stack and pushes False otherwise.
instructionVectorCharContains :: State -> State
instructionVectorCharContains = instructionVectorContains char vectorChar

-- |If the second to top char vector can be found within the first char vector from the
-- vector char stack, pushes True to the bool stack if is found, else False.
instructionVectorCharContainsVectorChar :: State -> State
instructionVectorCharContainsVectorChar = instructionVectorContainsVector vectorChar

-- |Finds the first index of the top char in the char stack inside of the
-- top char vector from the vector char stack and pushes the result to the int stack.
instructionVectorCharIndexOf :: State -> State
instructionVectorCharIndexOf = instructionVectorIndexOf char vectorChar

-- |Searches and pushes the index of the second char vector inside of the first
-- char vector to the int stack from the vector char stack. Pushes -1 if not found.
instructionVectorCharIndexOfVectorChar :: State -> State
instructionVectorCharIndexOfVectorChar = instructionVectorIndexOfVector vectorChar

-- |Finds the amount of times the top char on the char stack occurs inside of
-- the top char vector from the vector char stack and pushes the result to the
-- int stack.
instructionVectorCharOccurrencesOf :: State -> State
instructionVectorCharOccurrencesOf = instructionVectorOccurrencesOf char vectorChar

-- |Counts the amount of occurrences of the second char vector within the first
-- char vector. Pushes the result to the int stack.
instructionVectorCharOccurrencesOfVectorChar :: State -> State
instructionVectorCharOccurrencesOfVectorChar = instructionVectorOccurrencesOfVector vectorChar

-- |Splits the top char vector from the vector char stack into lists of size one and pushes
-- the result back one the vector char stack.
instructionVectorCharParseToChar :: State -> State
instructionVectorCharParseToChar = instructionVectorParseToPrim vectorChar

-- |Sets the Nth index inside of the top char vector from the vector char stack to the
-- top value from the primitive stack. N is pulled from the top of the int stack.
instructionVectorCharSetNth :: State -> State
instructionVectorCharSetNth = instructionVectorSetNth char vectorChar

-- |Splits the char vector on top of the vector char stack with the char from the top
-- of the char stack and pushes the result to the original vector stack.
instructionVectorCharSplitOn :: State -> State
instructionVectorCharSplitOn = instructionVectorSplitOn char vectorChar

-- |Splits the first char vector based on the second char vector from the vector
-- char stack and pushes the result to the vector char stack.
instructionVectorCharSplitOnVectorChar :: State -> State
instructionVectorCharSplitOnVectorChar = instructionVectorSplitOnVector vectorChar

-- |Replaces the first occurrence of the top char with the second char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack.
instructionVectorCharReplaceFirst :: State -> State
instructionVectorCharReplaceFirst = instructionVectorReplace char vectorChar (Just 1)

-- |Replaces all occurrences of the top char with the second char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack.
instructionVectorCharReplaceAll :: State -> State
instructionVectorCharReplaceAll = instructionVectorReplace char vectorChar Nothing

-- |Replaces N occurrences of the top char with the second char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack. N is pulled from
-- the top of the int stack.
instructionVectorCharReplaceN :: State -> State
instructionVectorCharReplaceN = instructionVectorReplaceN char vectorChar

-- |Replaces the first occurrence of the second char vector with the third char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack.
instructionVectorCharReplaceFirstVectorChar :: State -> State
instructionVectorCharReplaceFirstVectorChar = instructionVectorReplaceVector vectorChar (Just 1)

-- |Replaces all occurrences of the second char vector with the third char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack.
instructionVectorCharReplaceAllVectorChar :: State -> State
instructionVectorCharReplaceAllVectorChar = instructionVectorReplaceVector vectorChar Nothing

-- |Replaces N occurrences of the second char vector with the third char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack. N is pulled from the top of the int stack.
instructionVectorCharReplaceVectorCharN :: State -> State
instructionVectorCharReplaceVectorCharN = instructionVectorReplaceVectorN vectorChar

-- |Removes the first occurrence of the top char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack.
instructionVectorCharRemoveFirst :: State -> State
instructionVectorCharRemoveFirst = instructionVectorRemove char vectorChar (Just 1)

-- |Removes the all occurrences of the top char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack.
instructionVectorCharRemoveAll :: State -> State
instructionVectorCharRemoveAll = instructionVectorRemove char vectorChar Nothing

-- |Removes N occurrences of the top char from
-- the char stack inside of the top char vector from the vector char stack.
-- Pushes the modified char vector to the vector char stack. N is pulled
-- from the top of the int stack.
instructionVectorCharRemoveN :: State -> State
instructionVectorCharRemoveN = instructionVectorRemoveN char vectorChar

-- |Removes the first occurrence of the second char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack.
instructionVectorCharRemoveFirstVectorChar :: State -> State
instructionVectorCharRemoveFirstVectorChar = instructionVectorRemoveVector vectorChar (Just 1)

-- |Removes all occurrences of the second char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack.
instructionVectorCharRemoveAllVectorChar :: State -> State
instructionVectorCharRemoveAllVectorChar = instructionVectorRemoveVector vectorChar Nothing

-- |Removes N occurrences of the second char vector
-- inside of the first char vector from the vector char stack. Pushes the result to the
-- vector char stack. N is pulled from the top of the int stack.
instructionVectorCharRemoveNVectorChar :: State -> State
instructionVectorCharRemoveNVectorChar = instructionVectorRemoveVectorN vectorChar

-- |Iterates over the top char vector on the vector char stack, applying the top instruction of the
-- exec stack along the way.
instructionVectorCharIterate :: State -> State
instructionVectorCharIterate = instructionVectorIterate char vectorChar GeneVectorChar instructionVectorCharIterate "instructionVectorCharIterate"

-- |Sorts the top char vector on the vector char stack and pushes the result back to the
-- vector char stack.
instructionVectorCharSort :: State -> State
instructionVectorCharSort = instructionVectorSort vectorChar

-- |Sorts the top char vector on the vector char stack, reverses it, and pushes the result back to the
-- vector char stack.
instructionVectorCharSortReverse :: State -> State
instructionVectorCharSortReverse = instructionVectorSortReverse vectorChar

-- |Inserts the top char from the char stack into the top char vector from the
-- vector char stack at a specified index and pushes the result to the vector
-- char stack. The index is pulled from the top of the int stack.
instructionVectorCharInsert :: State -> State
instructionVectorCharInsert = instructionVectorInsert char vectorChar

-- |Inserts the second char vector into the first char vector from the vector char stack
-- at a specified index and pushes the result to the vector char stack. The index is
-- pulled from the top of the int stack.
instructionVectorCharInsertVectorChar :: State -> State
instructionVectorCharInsertVectorChar = instructionVectorInsertVector vectorChar
