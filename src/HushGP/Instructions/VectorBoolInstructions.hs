{-# LANGUAGE TemplateHaskell #-}
module HushGP.Instructions.VectorBoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
import HushGP.TH

-- |Pops the top bool vector from the bool vector stack.
instructionVectorBoolPop :: State -> State
instructionVectorBoolPop = instructionPop vectorBool

-- |Duplicates the top bool vector from the bool vector stack.
instructionVectorBoolDup :: State -> State
instructionVectorBoolDup = instructionDup vectorBool

-- |Duplicates the top bool vector from the bool vector stack N times
-- based on the top int from the int stack.
instructionVectorBoolDupN :: State -> State
instructionVectorBoolDupN = instructionDupN vectorBool

-- |Swaps the top two bool vectors from the bool vector stack.
instructionVectorBoolSwap :: State -> State
instructionVectorBoolSwap = instructionSwap vectorBool

-- |Rotates the top three bool vectors from the bool vector stack.
instructionVectorBoolRot :: State -> State
instructionVectorBoolRot = instructionRot vectorBool

-- |Sets the vector bool stack to []
instructionVectorBoolFlush :: State -> State
instructionVectorBoolFlush = instructionFlush vectorBool

-- |Pushes True to the bool stack if the top two bool vectors from
-- the vector bool stack are equal. Pushes False otherwise.
instructionVectorBoolEq :: State -> State
instructionVectorBoolEq = instructionEq vectorBool

-- |Calculates the size of the vector bool stack and pushes that number
-- to the int stack.
instructionVectorBoolStackDepth :: State -> State
instructionVectorBoolStackDepth = instructionStackDepth vectorBool

-- |Moves an item from deep within the vector bool stack to the top of the vector bool stack based on
-- the top int from the int stack.
instructionVectorBoolYank :: State -> State
instructionVectorBoolYank = instructionYank vectorBool

-- |Copies an item from deep within the vector bool stack to the top of the vector bool stack based on
-- the top int from the int stack.
instructionVectorBoolYankDup :: State -> State
instructionVectorBoolYankDup = instructionYankDup vectorBool

-- |Pushes True to the bool stack if the vector bool stack is empty. False if not.
instructionVectorBoolIsStackEmpty :: State -> State
instructionVectorBoolIsStackEmpty = instructionIsStackEmpty vectorBool

-- |Moves an item from the top of the vector bool stack to deep within the vector bool stack based on
-- the top int from the int stack.
instructionVectorBoolShove :: State -> State
instructionVectorBoolShove = instructionShove vectorBool

-- |Copies an item from the top of the vector bool stack to deep within the vector bool stack based on
-- the top int from the int stack.
instructionVectorBoolShoveDup :: State -> State
instructionVectorBoolShoveDup = instructionShoveDup vectorBool

-- |Duplicate the top N items from the vector bool stack based on the top int from the int stack.
instructionVectorBoolDupItems :: State -> State
instructionVectorBoolDupItems = instructionDupItems vectorBool

-- |Concats the top two vectors on top of the vector bool stack.
instructionVectorBoolConcat :: State -> State
instructionVectorBoolConcat = instructionVectorConcat vectorBool

-- |Takes the top bool from the bool stack and prepends it to top bool vector
-- on the bool vector stack.
instructionVectorBoolConj :: State -> State
instructionVectorBoolConj = instructionVectorConj bool vectorBool

-- |Takes the top bool from the bool stack and appends it to top bool vector
-- on the bool vector stack.
instructionVectorBoolConjEnd :: State -> State
instructionVectorBoolConjEnd = instructionVectorConjEnd bool vectorBool

-- |Takes the first N bools from the top of the bool vector from the bool vector
-- and pushes the result to the bool vector stack. N is pulled from the top of
-- the int stack.
instructionVectorBoolTakeN :: State -> State
instructionVectorBoolTakeN = instructionVectorTakeN vectorBool

-- |Takes the last N bools from the top of the bool vector from the bool vector
-- and pushes the result to the bool vector stack. N is pulled from the top of
-- the int stack.
instructionVectorBoolTakeRN :: State -> State
instructionVectorBoolTakeRN = instructionVectorTakeRN vectorBool

-- |Takes a sublist of the top bool vector on top of the vector bool stack.
-- The two ints to determine bounds are pulled from the top of the int stack.
instructionVectorBoolSubVector :: State -> State
instructionVectorBoolSubVector = instructionSubVector vectorBool

-- |Takes the first bool from the top of the vector bool stack and places
-- it on the bool stack.
instructionVectorBoolFirst :: State -> State
instructionVectorBoolFirst = instructionVectorFirst bool vectorBool

-- |Takes the first bool from the top of the vector bool stack and places
-- it wrapped in a list on top of the vector bool stack.
instructionVectorBoolFromFirstPrim :: State -> State
instructionVectorBoolFromFirstPrim = instructionVectorFromFirstPrim vectorBool

-- |Takes the first bool from the top of the bool stack and places it
-- wrapped in a list on top of the vector bool stack.
instructionVectorBoolFromPrim :: State -> State
instructionVectorBoolFromPrim = instructionVectorFromPrim bool vectorBool

-- |Takes the last bool from the top of the vector bool stack and places
-- it on the bool stack.
instructionVectorBoolLast :: State -> State
instructionVectorBoolLast = instructionVectorLast bool vectorBool

-- |Takes the last bool from the top bool vector on the vector bool stack and
-- places it on the bool stack.
instructionVectorBoolFromLastPrim :: State -> State
instructionVectorBoolFromLastPrim = instructionVectorFromLastPrim vectorBool

-- |Takes the Nth bool from the top bool vector and places it onto the bool stack
-- based on an int from the top of the int stack.
instructionVectorBoolNth :: State -> State
instructionVectorBoolNth = instructionVectorNth bool vectorBool

-- |Takes the Nth bool from the top bool vector on the vector bool stack and
-- creates a vector wrapping that Nth item, pushing it back onto the vector bool stack.
-- N is the top item on the int stack.
instructionVectorBoolFromNthPrim :: State -> State
instructionVectorBoolFromNthPrim = instructionVectorFromNthPrim vectorBool

-- |Removes the first bool from the top bool vector on the vector bool stack and
-- places the result back onto the vector bool stack.
instructionVectorBoolRest :: State -> State
instructionVectorBoolRest = instructionVectorRest vectorBool

-- |Removes the last bool from the top bool vector on the vector bool stack and
-- places the result back onto the vector bool stack.
instructionVectorBoolButLast :: State -> State
instructionVectorBoolButLast = instructionVectorButLast vectorBool

-- |Drops the first N items from the top bool vector and pushes the result
-- back to the vector bool stack. N is pulled from the top of the int stack.
instructionVectorBoolDrop :: State -> State
instructionVectorBoolDrop = instructionVectorDrop vectorBool

-- |Drops the last N items from the top bool vector and pushes the result
-- back to the vector bool stack. N is pulled from the top of the int stack.
instructionVectorBoolDropR :: State -> State
instructionVectorBoolDropR = instructionVectorDropR vectorBool

-- |Pushes the length of the top bool vector from the vector bool stack
-- to the top of the int stack.
instructionVectorBoolLength :: State -> State
instructionVectorBoolLength = instructionLength vectorBool

-- |Reverses the top bool vector from the vector bool stack and pushes the
-- result to the vector bool stack.
instructionVectorBoolReverse :: State -> State
instructionVectorBoolReverse = instructionReverse vectorBool

-- |Takes the top bool vector from the vector bool stack and pushes the
-- individual bools to the vector bool stack.
instructionVectorBoolPushAll :: State -> State
instructionVectorBoolPushAll = instructionPushAll bool vectorBool

-- |Makes an empty vector and pushes it to the vector bool stack.
instructionVectorBoolMakeEmpty :: State -> State
instructionVectorBoolMakeEmpty = instructionVectorMakeEmpty vectorBool

-- |Checks if the top bool vector from the vector bool stack is empty.
-- Pushes True if the bool vector is empty to the bool stack. False otherwise.
instructionVectorBoolIsEmpty :: State -> State
instructionVectorBoolIsEmpty = instructionVectorIsEmpty vectorBool

-- |If the top bool vector from the vector bool stack contains the top bool from the bool
-- stack, pushes True to the bool stack and pushes False otherwise.
instructionVectorBoolContains :: State -> State
instructionVectorBoolContains = instructionVectorContains bool vectorBool

-- |If the second to top bool vector can be found within the first bool vector from the
-- vector bool stack, pushes True to the bool stack if is found, else False.
instructionVectorBoolContainsVectorBool :: State -> State
instructionVectorBoolContainsVectorBool = instructionVectorContainsVector vectorBool

-- |Finds the first index of the top bool in the bool stack inside of the
-- top bool vector from the vector bool stack and pushes the result to the int stack.
instructionVectorBoolIndexOf :: State -> State
instructionVectorBoolIndexOf = instructionVectorIndexOf bool vectorBool

-- |Searches and pushes the index of the second bool vector inside of the first
-- bool vector to the int stack from the vector bool stack. Pushes -1 if not found.
instructionVectorBoolIndexOfVectorBool :: State -> State
instructionVectorBoolIndexOfVectorBool = instructionVectorIndexOfVector vectorBool

-- |Finds the amount of times the top bool on the bool stack occurs inside of
-- the top bool vector from the vector bool stack and pushes the result to the
-- int stack.
instructionVectorBoolOccurrencesOf :: State -> State
instructionVectorBoolOccurrencesOf = instructionVectorOccurrencesOf bool vectorBool

-- |Counts the amount of occurrences of the second bool vector within the first
-- bool vector. Pushes the result to the int stack.
instructionVectorBoolOccurrencesOfVectorBool :: State -> State
instructionVectorBoolOccurrencesOfVectorBool = instructionVectorOccurrencesOfVector vectorBool

-- |Splits the top bool vector from the vector bool stack into lists of size one and pushes
-- the result back one the vector bool stack.
instructionVectorBoolParseToBool :: State -> State
instructionVectorBoolParseToBool = instructionVectorParseToPrim vectorBool

-- |Sets the Nth index inside of the top bool vector from the vector bool stack to the
-- top value from the primitive stack. N is pulled from the top of the int stack.
instructionVectorBoolSetNth :: State -> State
instructionVectorBoolSetNth = instructionVectorSetNth bool vectorBool

-- |Splits the bool vector on top of the vector bool stack with the bool from the top
-- of the bool stack and pushes the result to the original vector stack.
instructionVectorBoolSplitOn :: State -> State
instructionVectorBoolSplitOn = instructionVectorSplitOn bool vectorBool

-- |Splits the first bool vector based on the second bool vector from the vector
-- bool stack and pushes the result to the vector bool stack.
instructionVectorBoolSplitOnVectorBool :: State -> State
instructionVectorBoolSplitOnVectorBool = instructionVectorSplitOnVector vectorBool

-- |Replaces the first occurrence of the top bool with the second bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack.
instructionVectorBoolReplaceFirst :: State -> State
instructionVectorBoolReplaceFirst = instructionVectorReplace bool vectorBool (Just 1)

-- |Replaces all occurrences of the top bool with the second bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack.
instructionVectorBoolReplaceAll :: State -> State
instructionVectorBoolReplaceAll = instructionVectorReplace bool vectorBool Nothing

-- |Replaces N occurrences of the top bool with the second bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack. N is pulled from
-- the top of the int stack.
instructionVectorBoolReplaceN :: State -> State
instructionVectorBoolReplaceN = instructionVectorReplaceN bool vectorBool

-- |Replaces the first occurrence of the second bool vector with the third bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack.
instructionVectorBoolReplaceFirstVectorBool :: State -> State
instructionVectorBoolReplaceFirstVectorBool = instructionVectorReplaceVector vectorBool (Just 1)

-- |Replaces all occurrences of the second bool vector with the third bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack.
instructionVectorBoolReplaceAllVectorBool :: State -> State
instructionVectorBoolReplaceAllVectorBool = instructionVectorReplaceVector vectorBool Nothing

-- |Replaces N occurrences of the second bool vector with the third bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack. N is pulled from the top of the int stack.
instructionVectorBoolReplaceVectorBoolN :: State -> State
instructionVectorBoolReplaceVectorBoolN = instructionVectorReplaceVectorN vectorBool

-- |Removes the first occurrence of the top bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack.
instructionVectorBoolRemoveFirst :: State -> State
instructionVectorBoolRemoveFirst = instructionVectorRemove bool vectorBool (Just 1)

-- |Removes the all occurrences of the top bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack.
instructionVectorBoolRemoveAll :: State -> State
instructionVectorBoolRemoveAll = instructionVectorRemove bool vectorBool Nothing

-- |Removes N occurrences of the top bool from
-- the bool stack inside of the top bool vector from the vector bool stack.
-- Pushes the modified bool vector to the vector bool stack. N is pulled
-- from the top of the int stack.
instructionVectorBoolRemoveN :: State -> State
instructionVectorBoolRemoveN = instructionVectorRemoveN bool vectorBool

-- |Removes the first occurrence of the second bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack.
instructionVectorBoolRemoveFirstVectorBool :: State -> State
instructionVectorBoolRemoveFirstVectorBool = instructionVectorRemoveVector vectorBool (Just 1)

-- |Removes all occurrences of the second bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack.
instructionVectorBoolRemoveAllVectorBool :: State -> State
instructionVectorBoolRemoveAllVectorBool = instructionVectorRemoveVector vectorBool Nothing

-- |Removes N occurrences of the second bool vector
-- inside of the first bool vector from the vector bool stack. Pushes the result to the
-- vector bool stack. N is pulled from the top of the int stack.
instructionVectorBoolRemoveNVectorBool :: State -> State
instructionVectorBoolRemoveNVectorBool = instructionVectorRemoveVectorN vectorBool

-- |Iterates over the top bool vector on the vector bool stack, applying the top instruction of the
-- exec stack along the way.
instructionVectorBoolIterate :: State -> State
instructionVectorBoolIterate = instructionVectorIterate bool vectorBool GeneVectorBool instructionVectorBoolIterate "instructionVectorBoolIterate"

-- |Sorts the top bool vector on the vector bool stack and pushes the result back to the
-- vector bool stack.
instructionVectorBoolSort :: State -> State
instructionVectorBoolSort = instructionVectorSort vectorBool

-- |Sorts the top bool vector on the vector bool stack, reverses it, and pushes the result back to the
-- vector bool stack.
instructionVectorBoolSortReverse :: State -> State
instructionVectorBoolSortReverse = instructionVectorSortReverse vectorBool

-- |Inserts the top bool from the bool stack into the top bool vector from the
-- vector bool stack at a specified index and pushes the result to the vector
-- bool stack. The index is pulled from the top of the int stack.
instructionVectorBoolInsert :: State -> State
instructionVectorBoolInsert = instructionVectorInsert bool vectorBool

-- |Inserts the second bool vector into the first bool vector from the vector bool stack
-- at a specified index and pushes the result to the vector bool stack. The index is
-- pulled from the top of the int stack.
instructionVectorBoolInsertVectorBool :: State -> State
instructionVectorBoolInsertVectorBool = instructionVectorInsertVector vectorBool

allVectorBoolInstructions :: [Gene]
allVectorBoolInstructions = map StateFunc ($(functionExtractor "instruction"))
