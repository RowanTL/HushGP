{-# LANGUAGE TemplateHaskell #-}
module HushGP.Instructions.VectorIntInstructions where

import HushGP.Instructions.GenericInstructions
import HushGP.State
import HushGP.TH

-- |Pops the top int vector from the int vector stack.
instructionVectorIntPop :: State -> State
instructionVectorIntPop = instructionPop vectorInt

-- |Duplicates the top int vector from the int vector stack.
instructionVectorIntDup :: State -> State
instructionVectorIntDup = instructionDup vectorInt

-- |Duplicates the top int vector from the int vector stack N times
-- based on the top int from the int stack.
instructionVectorIntDupN :: State -> State
instructionVectorIntDupN = instructionDupN vectorInt

-- |Swaps the top two int vectors from the int vector stack.
instructionVectorIntSwap :: State -> State
instructionVectorIntSwap = instructionSwap vectorInt

-- |Rotates the top three int vectors from the int vector stack.
instructionVectorIntRot :: State -> State
instructionVectorIntRot = instructionRot vectorInt

-- |Sets the vector int stack to []
instructionVectorIntFlush :: State -> State
instructionVectorIntFlush = instructionFlush vectorInt

-- |Pushes True to the bool stack if the top two int vectors from
-- the vector int stack are equal. Pushes False otherwise.
instructionVectorIntEq :: State -> State
instructionVectorIntEq = instructionEq vectorInt

-- |Calculates the size of the vector int stack and pushes that number
-- to the int stack.
instructionVectorIntStackDepth :: State -> State
instructionVectorIntStackDepth = instructionStackDepth vectorInt

-- |Moves an item from deep within the vector int stack to the top of the vector int stack based on
-- the top int from the int stack.
instructionVectorIntYank :: State -> State
instructionVectorIntYank = instructionYank vectorInt

-- |Copies an item from deep within the vector int stack to the top of the vector int stack based on
-- the top int from the int stack.
instructionVectorIntYankDup :: State -> State
instructionVectorIntYankDup = instructionYankDup vectorInt

-- |Pushes True to the bool stack if the vector int stack is empty. False if not.
instructionVectorIntIsStackEmpty :: State -> State
instructionVectorIntIsStackEmpty = instructionIsStackEmpty vectorInt

-- |Moves an item from the top of the vector int stack to deep within the vector int stack based on
-- the top int from the int stack.
instructionVectorIntShove :: State -> State
instructionVectorIntShove = instructionShove vectorInt

-- |Copies an item from the top of the vector int stack to deep within the vector int stack based on
-- the top int from the int stack.
instructionVectorIntShoveDup :: State -> State
instructionVectorIntShoveDup = instructionShoveDup vectorInt

-- |Duplicate the top N items from the vector int stack based on the top int from the int stack.
instructionVectorIntDupItems :: State -> State
instructionVectorIntDupItems = instructionDupItems vectorInt

-- |Concats the top two vectors on top of the vector int stack.
instructionVectorIntConcat :: State -> State
instructionVectorIntConcat = instructionVectorConcat vectorInt

-- |Takes the top int from the int stack and prepends it to top int vector
-- on the int vector stack.
instructionVectorIntConj :: State -> State
instructionVectorIntConj = instructionVectorConj int vectorInt

-- |Takes the top int from the int stack and appends it to top int vector
-- on the int vector stack.
instructionVectorIntConjEnd :: State -> State
instructionVectorIntConjEnd = instructionVectorConjEnd int vectorInt

-- |Takes the first N ints from the top of the int vector from the int vector
-- and pushes the result to the int vector stack. N is pulled from the top of
-- the int stack.
instructionVectorIntTakeN :: State -> State
instructionVectorIntTakeN = instructionVectorTakeN vectorInt

-- |Takes the last N ints from the top of the int vector from the int vector
-- and pushes the result to the int vector stack. N is pulled from the top of
-- the int stack.
instructionVectorIntTakeRN :: State -> State
instructionVectorIntTakeRN = instructionVectorTakeRN vectorInt

-- |Takes a sublist of the top int vector on top of the vector int stack.
-- The two ints to determine bounds are pulled from the top of the int stack.
instructionVectorIntSubVector :: State -> State
instructionVectorIntSubVector = instructionSubVector vectorInt

-- |Takes the first int from the top of the vector int stack and places
-- it on the int stack.
instructionVectorIntFirst :: State -> State
instructionVectorIntFirst = instructionVectorFirst int vectorInt

-- |Takes the first int from the top of the vector int stack and places
-- it wrapped in a list on top of the vector int stack.
instructionVectorIntFromFirstPrim :: State -> State
instructionVectorIntFromFirstPrim = instructionVectorFromFirstPrim vectorInt

-- |Takes the first int from the top of the int stack and places it
-- wrapped in a list on top of the vector int stack.
instructionVectorIntFromPrim :: State -> State
instructionVectorIntFromPrim = instructionVectorFromPrim int vectorInt

-- |Takes the last int from the top of the vector int stack and places
-- it on the int stack.
instructionVectorIntLast :: State -> State
instructionVectorIntLast = instructionVectorLast int vectorInt

-- |Takes the last int from the top int vector on the vector int stack and
-- places it on the int stack.
instructionVectorIntFromLastPrim :: State -> State
instructionVectorIntFromLastPrim = instructionVectorFromLastPrim vectorInt

-- |Takes the Nth int from the top int vector and places it onto the int stack
-- based on an int from the top of the int stack.
instructionVectorIntNth :: State -> State
instructionVectorIntNth = instructionVectorNth int vectorInt

-- |Takes the Nth int from the top int vector on the vector int stack and
-- creates a vector wrapping that Nth item, pushing it back onto the vector int stack.
-- N is the top item on the int stack.
instructionVectorIntFromNthPrim :: State -> State
instructionVectorIntFromNthPrim = instructionVectorFromNthPrim vectorInt

-- |Removes the first int from the top int vector on the vector int stack and
-- places the result back onto the vector int stack.
instructionVectorIntRest :: State -> State
instructionVectorIntRest = instructionVectorRest vectorInt

-- |Removes the last int from the top int vector on the vector int stack and
-- places the result back onto the vector int stack.
instructionVectorIntButLast :: State -> State
instructionVectorIntButLast = instructionVectorButLast vectorInt

-- |Drops the first N items from the top int vector and pushes the result
-- back to the vector int stack. N is pulled from the top of the int stack.
instructionVectorIntDrop :: State -> State
instructionVectorIntDrop = instructionVectorDrop vectorInt

-- |Drops the last N items from the top int vector and pushes the result
-- back to the vector int stack. N is pulled from the top of the int stack.
instructionVectorIntDropR :: State -> State
instructionVectorIntDropR = instructionVectorDropR vectorInt

-- |Pushes the length of the top int vector from the vector int stack
-- to the top of the int stack.
instructionVectorIntLength :: State -> State
instructionVectorIntLength = instructionLength vectorInt

-- |Reverses the top int vector from the vector int stack and pushes the
-- result to the vector int stack.
instructionVectorIntReverse :: State -> State
instructionVectorIntReverse = instructionReverse vectorInt

-- |Takes the top int vector from the vector int stack and pushes the
-- individual ints to the vector int stack.
instructionVectorIntPushAll :: State -> State
instructionVectorIntPushAll = instructionPushAll int vectorInt

-- |Makes an empty vector and pushes it to the vector int stack.
instructionVectorIntMakeEmpty :: State -> State
instructionVectorIntMakeEmpty = instructionVectorMakeEmpty vectorInt

-- |Checks if the top int vector from the vector int stack is empty.
-- Pushes True if the int vector is empty to the bool stack. False otherwise.
instructionVectorIntIsEmpty :: State -> State
instructionVectorIntIsEmpty = instructionVectorIsEmpty vectorInt

-- |If the top int vector from the vector int stack contains the top int from the int
-- stack, pushes True to the bool stack and pushes False otherwise.
instructionVectorIntContains :: State -> State
instructionVectorIntContains = instructionVectorContains int vectorInt

-- |If the second to top int vector can be found within the first int vector from the
-- vector int stack, pushes True to the bool stack if is found, else False.
instructionVectorIntContainsVectorInt :: State -> State
instructionVectorIntContainsVectorInt = instructionVectorContainsVector vectorInt

-- |Finds the first index of the top int in the int stack inside of the
-- top int vector from the vector int stack and pushes the result to the int stack.
instructionVectorIntIndexOf :: State -> State
instructionVectorIntIndexOf = instructionVectorIndexOf int vectorInt

-- |Searches and pushes the index of the second int vector inside of the first
-- int vector to the int stack from the vector int stack. Pushes -1 if not found.
instructionVectorIntIndexOfVectorInt :: State -> State
instructionVectorIntIndexOfVectorInt = instructionVectorIndexOfVector vectorInt

-- |Finds the amount of times the top int on the int stack occurs inside of
-- the top int vector from the vector int stack and pushes the result to the
-- int stack.
instructionVectorIntOccurrencesOf :: State -> State
instructionVectorIntOccurrencesOf = instructionVectorOccurrencesOf int vectorInt

-- |Counts the amount of occurrences of the second int vector within the first
-- int vector. Pushes the result to the int stack.
instructionVectorIntOccurrencesOfVectorInt :: State -> State
instructionVectorIntOccurrencesOfVectorInt = instructionVectorOccurrencesOfVector vectorInt

-- |Splits the top int vector from the vector int stack into lists of size one and pushes
-- the result back one the vector int stack.
instructionVectorIntParseToInt :: State -> State
instructionVectorIntParseToInt = instructionVectorParseToPrim vectorInt

-- |Sets the Nth index inside of the top int vector from the vector int stack to the
-- top value from the primitive stack. N is pulled from the top of the int stack.
instructionVectorIntSetNth :: State -> State
instructionVectorIntSetNth = instructionVectorSetNth int vectorInt

-- |Splits the int vector on top of the vector int stack with the int from the top
-- of the int stack and pushes the result to the original vector stack.
instructionVectorIntSplitOn :: State -> State
instructionVectorIntSplitOn = instructionVectorSplitOn int vectorInt

-- |Splits the first int vector based on the second int vector from the vector
-- int stack and pushes the result to the vector int stack.
instructionVectorIntSplitOnVectorInt :: State -> State
instructionVectorIntSplitOnVectorInt = instructionVectorSplitOnVector vectorInt

-- |Replaces the first occurrence of the top int with the second int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack.
instructionVectorIntReplaceFirst :: State -> State
instructionVectorIntReplaceFirst = instructionVectorReplace int vectorInt (Just 1)

-- |Replaces all occurrences of the top int with the second int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack.
instructionVectorIntReplaceAll :: State -> State
instructionVectorIntReplaceAll = instructionVectorReplace int vectorInt Nothing

-- |Replaces N occurrences of the top int with the second int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack. N is pulled from
-- the top of the int stack.
instructionVectorIntReplaceN :: State -> State
instructionVectorIntReplaceN = instructionVectorReplaceN int vectorInt

-- |Replaces the first occurrence of the second int vector with the third int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack.
instructionVectorIntReplaceFirstVectorInt :: State -> State
instructionVectorIntReplaceFirstVectorInt = instructionVectorReplaceVector vectorInt (Just 1)

-- |Replaces all occurrences of the second int vector with the third int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack.
instructionVectorIntReplaceAllVectorInt :: State -> State
instructionVectorIntReplaceAllVectorInt = instructionVectorReplaceVector vectorInt Nothing

-- |Replaces N occurrences of the second int vector with the third int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack. N is pulled from the top of the int stack.
instructionVectorIntReplaceVectorIntN :: State -> State
instructionVectorIntReplaceVectorIntN = instructionVectorReplaceVectorN vectorInt

-- |Removes the first occurrence of the top int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack.
instructionVectorIntRemoveFirst :: State -> State
instructionVectorIntRemoveFirst = instructionVectorRemove int vectorInt (Just 1)

-- |Removes the all occurrences of the top int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack.
instructionVectorIntRemoveAll :: State -> State
instructionVectorIntRemoveAll = instructionVectorRemove int vectorInt Nothing

-- |Removes N occurrences of the top int from
-- the int stack inside of the top int vector from the vector int stack.
-- Pushes the modified int vector to the vector int stack. N is pulled
-- from the top of the int stack.
instructionVectorIntRemoveN :: State -> State
instructionVectorIntRemoveN = instructionVectorRemoveN int vectorInt

-- |Removes the first occurrence of the second int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack.
instructionVectorIntRemoveFirstVectorInt :: State -> State
instructionVectorIntRemoveFirstVectorInt = instructionVectorRemoveVector vectorInt (Just 1)

-- |Removes all occurrences of the second int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack.
instructionVectorIntRemoveAllVectorInt :: State -> State
instructionVectorIntRemoveAllVectorInt = instructionVectorRemoveVector vectorInt Nothing

-- |Removes N occurrences of the second int vector
-- inside of the first int vector from the vector int stack. Pushes the result to the
-- vector int stack. N is pulled from the top of the int stack.
instructionVectorIntRemoveNVectorInt :: State -> State
instructionVectorIntRemoveNVectorInt = instructionVectorRemoveVectorN vectorInt

-- |Iterates over the top int vector on the vector int stack, applying the top instruction of the
-- exec stack along the way.
instructionVectorIntIterate :: State -> State
instructionVectorIntIterate = instructionVectorIterate int vectorInt GeneVectorInt instructionVectorIntIterate "instructionVectorIntIterate"

-- |Sorts the top int vector on the vector int stack and pushes the result back to the
-- vector int stack.
instructionVectorIntSort :: State -> State
instructionVectorIntSort = instructionVectorSort vectorInt

-- |Sorts the top int vector on the vector int stack, reverses it, and pushes the result back to the
-- vector int stack.
instructionVectorIntSortReverse :: State -> State
instructionVectorIntSortReverse = instructionVectorSortReverse vectorInt

-- |Inserts the top int from the int stack into the top int vector from the
-- vector int stack at a specified index and pushes the result to the vector
-- int stack. The index is pulled from the top of the int stack.
instructionVectorIntInsert :: State -> State
instructionVectorIntInsert = instructionVectorInsert int vectorInt

-- |Inserts the second int vector into the first int vector from the vector int stack
-- at a specified index and pushes the result to the vector int stack. The index is
-- pulled from the top of the int stack.
instructionVectorIntInsertVectorInt :: State -> State
instructionVectorIntInsertVectorInt = instructionVectorInsertVector vectorInt

allVectorIntInstructions :: [Gene]
allVectorIntInstructions = map StateFunc ($(functionExtractor "instruction"))
