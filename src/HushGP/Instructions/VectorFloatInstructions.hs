{-# LANGUAGE TemplateHaskell #-}
module HushGP.Instructions.VectorFloatInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions
import HushGP.TH
import HushGP.Instructions.Utility

-- |Pops the top float vector from the float vector stack.
instructionVectorFloatPop :: State -> State
instructionVectorFloatPop = instructionPop vectorFloat

-- |Duplicates the top float vector from the float vector stack.
instructionVectorFloatDup :: State -> State
instructionVectorFloatDup = instructionDup vectorFloat

-- |Duplicates the top float vector from the float vector stack N times
-- based on the top int from the int stack.
instructionVectorFloatDupN :: State -> State
instructionVectorFloatDupN = instructionDupN vectorFloat

-- |Swaps the top two float vectors from the float vector stack.
instructionVectorFloatSwap :: State -> State
instructionVectorFloatSwap = instructionSwap vectorFloat

-- |Rotates the top three float vectors from the float vector stack.
instructionVectorFloatRot :: State -> State
instructionVectorFloatRot = instructionRot vectorFloat

-- |Sets the vector float stack to []
instructionVectorFloatFlush :: State -> State
instructionVectorFloatFlush = instructionFlush vectorFloat

-- |Pushes True to the bool stack if the top two float vectors from
-- the vector float stack are equal. Pushes False otherwise.
instructionVectorFloatEq :: State -> State
instructionVectorFloatEq = instructionEq vectorFloat

-- |Calculates the size of the vector float stack and pushes that number
-- to the int stack.
instructionVectorFloatStackDepth :: State -> State
instructionVectorFloatStackDepth = instructionStackDepth vectorFloat

-- |Moves an item from deep within the vector float stack to the top of the vector float stack based on
-- the top int from the int stack.
instructionVectorFloatYank :: State -> State
instructionVectorFloatYank = instructionYank vectorFloat

-- |Copies an item from deep within the vector float stack to the top of the vector float stack based on
-- the top int from the int stack.
instructionVectorFloatYankDup :: State -> State
instructionVectorFloatYankDup = instructionYankDup vectorFloat

-- |Pushes True to the bool stack if the vector float stack is empty. False if not.
instructionVectorFloatIsStackEmpty :: State -> State
instructionVectorFloatIsStackEmpty = instructionIsStackEmpty vectorFloat

-- |Moves an item from the top of the vector float stack to deep within the vector float stack based on
-- the top int from the int stack.
instructionVectorFloatShove :: State -> State
instructionVectorFloatShove = instructionShove vectorFloat

-- |Copies an item from the top of the vector float stack to deep within the vector float stack based on
-- the top int from the int stack.
instructionVectorFloatShoveDup :: State -> State
instructionVectorFloatShoveDup = instructionShoveDup vectorFloat

-- |Duplicate the top N items from the vector float stack based on the top int from the int stack.
instructionVectorFloatDupItems :: State -> State
instructionVectorFloatDupItems = instructionDupItems vectorFloat

-- |Concats the top two vectors on top of the vector float stack.
instructionVectorFloatConcat :: State -> State
instructionVectorFloatConcat = instructionVectorConcat vectorFloat

-- |Takes the top float from the float stack and prepends it to top float vector
-- on the float vector stack.
instructionVectorFloatConj :: State -> State
instructionVectorFloatConj = instructionVectorConj float vectorFloat

-- |Takes the top float from the float stack and appends it to top float vector
-- on the float vector stack.
instructionVectorFloatConjEnd :: State -> State
instructionVectorFloatConjEnd = instructionVectorConjEnd float vectorFloat

-- |Takes the first N floats from the top of the float vector from the float vector
-- and pushes the result to the float vector stack. N is pulled from the top of
-- the int stack.
instructionVectorFloatTakeN :: State -> State
instructionVectorFloatTakeN = instructionVectorTakeN vectorFloat

-- |Takes the last N floats from the top of the float vector from the float vector
-- and pushes the result to the float vector stack. N is pulled from the top of
-- the int stack.
instructionVectorFloatTakeRN :: State -> State
instructionVectorFloatTakeRN = instructionVectorTakeRN vectorFloat

-- |Takes a sublist of the top float vector on top of the vector float stack.
-- The two ints to determine bounds are pulled from the top of the int stack.
instructionVectorFloatSubVector :: State -> State
instructionVectorFloatSubVector = instructionSubVector vectorFloat

-- |Takes the first float from the top of the vector float stack and places
-- it on the float stack.
instructionVectorFloatFirst :: State -> State
instructionVectorFloatFirst = instructionVectorFirst float vectorFloat

-- |Takes the first float from the top of the vector float stack and places
-- it wrapped in a list on top of the vector float stack.
instructionVectorFloatFromFirstPrim :: State -> State
instructionVectorFloatFromFirstPrim = instructionVectorFromFirstPrim vectorFloat

-- |Takes the first float from the top of the float stack and places it
-- wrapped in a list on top of the vector float stack.
instructionVectorFloatFromPrim :: State -> State
instructionVectorFloatFromPrim = instructionVectorFromPrim float vectorFloat

-- |Takes the last float from the top of the vector float stack and places
-- it on the float stack.
instructionVectorFloatLast :: State -> State
instructionVectorFloatLast = instructionVectorLast float vectorFloat

-- |Takes the last float from the top float vector on the vector float stack and
-- places it on the float stack.
instructionVectorFloatFromLastPrim :: State -> State
instructionVectorFloatFromLastPrim = instructionVectorFromLastPrim vectorFloat

-- |Takes the Nth float from the top float vector and places it onto the float stack
-- based on an int from the top of the int stack.
instructionVectorFloatNth :: State -> State
instructionVectorFloatNth = instructionVectorNth float vectorFloat

-- |Takes the Nth float from the top float vector on the vector float stack and
-- creates a vector wrapping that Nth item, pushing it back onto the vector float stack.
-- N is the top item on the int stack.
instructionVectorFloatFromNthPrim :: State -> State
instructionVectorFloatFromNthPrim = instructionVectorFromNthPrim vectorFloat

-- |Removes the first float from the top float vector on the vector float stack and
-- places the result back onto the vector float stack.
instructionVectorFloatRest :: State -> State
instructionVectorFloatRest = instructionVectorRest vectorFloat

-- |Removes the last float from the top float vector on the vector float stack and
-- places the result back onto the vector float stack.
instructionVectorFloatButLast :: State -> State
instructionVectorFloatButLast = instructionVectorButLast vectorFloat

-- |Drops the first N items from the top float vector and pushes the result
-- back to the vector float stack. N is pulled from the top of the int stack.
instructionVectorFloatDrop :: State -> State
instructionVectorFloatDrop = instructionVectorDrop vectorFloat

-- |Drops the last N items from the top float vector and pushes the result
-- back to the vector float stack. N is pulled from the top of the int stack.
instructionVectorFloatDropR :: State -> State
instructionVectorFloatDropR = instructionVectorDropR vectorFloat

-- |Pushes the length of the top float vector from the vector float stack
-- to the top of the int stack.
instructionVectorFloatLength :: State -> State
instructionVectorFloatLength = instructionLength vectorFloat

-- |Reverses the top float vector from the vector float stack and pushes the
-- result to the vector float stack.
instructionVectorFloatReverse :: State -> State
instructionVectorFloatReverse = instructionReverse vectorFloat

-- |Takes the top float vector from the vector float stack and pushes the
-- individual floats to the vector float stack.
instructionVectorFloatPushAll :: State -> State
instructionVectorFloatPushAll = instructionPushAll float vectorFloat

-- |Makes an empty vector and pushes it to the vector float stack.
instructionVectorFloatMakeEmpty :: State -> State
instructionVectorFloatMakeEmpty = instructionVectorMakeEmpty vectorFloat

-- |Checks if the top float vector from the vector float stack is empty.
-- Pushes True if the float vector is empty to the bool stack. False otherwise.
instructionVectorFloatIsEmpty :: State -> State
instructionVectorFloatIsEmpty = instructionVectorIsEmpty vectorFloat

-- |If the top float vector from the vector float stack contains the top float from the float
-- stack, pushes True to the bool stack and pushes False otherwise.
instructionVectorFloatContains :: State -> State
instructionVectorFloatContains = instructionVectorContains float vectorFloat

-- |If the second to top float vector can be found within the first float vector from the
-- vector float stack, pushes True to the bool stack if is found, else False.
instructionVectorFloatContainsVectorFloat :: State -> State
instructionVectorFloatContainsVectorFloat = instructionVectorContainsVector vectorFloat

-- |Finds the first index of the top float in the float stack inside of the
-- top float vector from the vector float stack and pushes the result to the int stack.
instructionVectorFloatIndexOf :: State -> State
instructionVectorFloatIndexOf = instructionVectorIndexOf float vectorFloat

-- |Searches and pushes the index of the second float vector inside of the first
-- float vector to the int stack from the vector float stack. Pushes -1 if not found.
instructionVectorFloatIndexOfVectorFloat :: State -> State
instructionVectorFloatIndexOfVectorFloat = instructionVectorIndexOfVector vectorFloat

-- |Finds the amount of times the top float on the float stack occurs inside of
-- the top float vector from the vector float stack and pushes the result to the
-- int stack.
instructionVectorFloatOccurrencesOf :: State -> State
instructionVectorFloatOccurrencesOf = instructionVectorOccurrencesOf float vectorFloat

-- |Counts the amount of occurrences of the second float vector within the first
-- float vector. Pushes the result to the int stack.
instructionVectorFloatOccurrencesOfVectorFloat :: State -> State
instructionVectorFloatOccurrencesOfVectorFloat = instructionVectorOccurrencesOfVector vectorFloat

-- |Splits the top float vector from the vector float stack into lists of size one and pushes
-- the result back one the vector float stack.
instructionVectorFloatParseToFloat :: State -> State
instructionVectorFloatParseToFloat = instructionVectorParseToPrim vectorFloat

-- |Sets the Nth index inside of the top float vector from the vector float stack to the
-- top value from the primitive stack. N is pulled from the top of the int stack.
instructionVectorFloatSetNth :: State -> State
instructionVectorFloatSetNth = instructionVectorSetNth float vectorFloat

-- |Splits the float vector on top of the vector float stack with the float from the top
-- of the float stack and pushes the result to the original vector stack.
instructionVectorFloatSplitOn :: State -> State
instructionVectorFloatSplitOn = instructionVectorSplitOn float vectorFloat

-- |Splits the first float vector based on the second float vector from the vector
-- float stack and pushes the result to the vector float stack.
instructionVectorFloatSplitOnVectorFloat :: State -> State
instructionVectorFloatSplitOnVectorFloat = instructionVectorSplitOnVector vectorFloat

-- |Replaces the first occurrence of the top float with the second float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack.
instructionVectorFloatReplaceFirst :: State -> State
instructionVectorFloatReplaceFirst = instructionVectorReplace float vectorFloat (Just 1)

-- |Replaces all occurrences of the top float with the second float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack.
instructionVectorFloatReplaceAll :: State -> State
instructionVectorFloatReplaceAll = instructionVectorReplace float vectorFloat Nothing

-- |Replaces N occurrences of the top float with the second float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack. N is pulled from
-- the top of the int stack.
instructionVectorFloatReplaceN :: State -> State
instructionVectorFloatReplaceN = instructionVectorReplaceN float vectorFloat

-- |Replaces the first occurrence of the second float vector with the third float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack.
instructionVectorFloatReplaceFirstVectorFloat :: State -> State
instructionVectorFloatReplaceFirstVectorFloat = instructionVectorReplaceVector vectorFloat (Just 1)

-- |Replaces all occurrences of the second float vector with the third float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack.
instructionVectorFloatReplaceAllVectorFloat :: State -> State
instructionVectorFloatReplaceAllVectorFloat = instructionVectorReplaceVector vectorFloat Nothing

-- |Replaces N occurrences of the second float vector with the third float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack. N is pulled from the top of the int stack.
instructionVectorFloatReplaceVectorFloatN :: State -> State
instructionVectorFloatReplaceVectorFloatN = instructionVectorReplaceVectorN vectorFloat

-- |Removes the first occurrence of the top float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack.
instructionVectorFloatRemoveFirst :: State -> State
instructionVectorFloatRemoveFirst = instructionVectorRemove float vectorFloat (Just 1)

-- |Removes the all occurrences of the top float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack.
instructionVectorFloatRemoveAll :: State -> State
instructionVectorFloatRemoveAll = instructionVectorRemove float vectorFloat Nothing

-- |Removes N occurrences of the top float from
-- the float stack inside of the top float vector from the vector float stack.
-- Pushes the modified float vector to the vector float stack. N is pulled
-- from the top of the int stack.
instructionVectorFloatRemoveN :: State -> State
instructionVectorFloatRemoveN = instructionVectorRemoveN float vectorFloat

-- |Removes the first occurrence of the second float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack.
instructionVectorFloatRemoveFirstVectorFloat :: State -> State
instructionVectorFloatRemoveFirstVectorFloat = instructionVectorRemoveVector vectorFloat (Just 1)

-- |Removes all occurrences of the second float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack.
instructionVectorFloatRemoveAllVectorFloat :: State -> State
instructionVectorFloatRemoveAllVectorFloat = instructionVectorRemoveVector vectorFloat Nothing

-- |Removes N occurrences of the second float vector
-- inside of the first float vector from the vector float stack. Pushes the result to the
-- vector float stack. N is pulled from the top of the int stack.
instructionVectorFloatRemoveNVectorFloat :: State -> State
instructionVectorFloatRemoveNVectorFloat = instructionVectorRemoveVectorN vectorFloat

-- |Iterates over the top float vector on the vector float stack, applying the top instruction of the
-- exec stack along the way.
instructionVectorFloatIterate :: State -> State
instructionVectorFloatIterate = instructionVectorIterate float vectorFloat GeneVectorFloat instructionVectorFloatIterate "instructionVectorFloatIterate"

-- |Sorts the top float vector on the vector float stack and pushes the result back to the
-- vector float stack.
instructionVectorFloatSort :: State -> State
instructionVectorFloatSort = instructionVectorSort vectorFloat

-- |Sorts the top float vector on the vector float stack, reverses it, and pushes the result back to the
-- vector float stack.
instructionVectorFloatSortReverse :: State -> State
instructionVectorFloatSortReverse = instructionVectorSortReverse vectorFloat

-- |Inserts the top float from the float stack into the top float vector from the
-- vector float stack at a specified index and pushes the result to the vector
-- float stack. The index is pulled from the top of the int stack.
instructionVectorFloatInsert :: State -> State
instructionVectorFloatInsert = instructionVectorInsert float vectorFloat

-- |Inserts the second float vector into the first float vector from the vector float stack
-- at a specified index and pushes the result to the vector float stack. The index is
-- pulled from the top of the int stack.
instructionVectorFloatInsertVectorFloat :: State -> State
instructionVectorFloatInsertVectorFloat = instructionVectorInsertVector vectorFloat

-- |Takes the mean of the top float vector and pushes the rounded float value
-- to the float stack.
instructionVectorFloatMean :: State -> State
instructionVectorFloatMean state@(State {_vectorFloat = [] : vs}) = instructionVectorFuncVectorToPrim float vectorFloat retZero state
instructionVectorFloatMean state = instructionVectorFuncVectorToPrim float vectorFloat (\xs -> sum xs / fromIntegral @Int @Double (length xs)) state

-- |Takes the maximum of the top float vector and pushes the float value
-- to the float stack.
instructionVectorFloatMaximum :: State -> State
instructionVectorFloatMaximum state@(State {_vectorFloat = [] : _}) = instructionVectorFuncVectorToPrim float vectorFloat retZero state
instructionVectorFloatMaximum state = instructionVectorFuncVectorToPrim float vectorFloat maximum state

-- |Takes the minimum of the top float vector and pushes the float value
-- to the float stack.
instructionVectorFloatMinimum :: State -> State
instructionVectorFloatMinimum state@(State {_vectorFloat = [] : _ }) = instructionVectorFuncVectorToPrim float vectorFloat retZero state
instructionVectorFloatMinimum state = instructionVectorFuncVectorToPrim float vectorFloat minimum state

-- |Takes the sum of the top float vector and pushes the float value
-- to the float stack.
instructionVectorFloatSum :: State -> State
instructionVectorFloatSum state@(State {_vectorFloat = [] : _}) = instructionVectorFuncVectorToPrim float vectorFloat retZero state
instructionVectorFloatSum state = instructionVectorFuncVectorToPrim float vectorFloat sum state

-- |Takes the mode of the top float vector and pushes the float value
-- to the float stack.
instructionVectorFloatMode :: State -> State
instructionVectorFloatMode state@(State {_vectorFloat = [] : _}) = instructionVectorFuncVectorToPrim float vectorFloat retZero state
instructionVectorFloatMode state = instructionVectorFuncVectorToPrim float vectorFloat mode state

-- |Takes the 2-norm of the top float vector and pushes the rounded result to
-- the float stack.
instructionVectorFloatNorm :: State -> State -- Ends up replacing with 0 so it's good.
instructionVectorFloatNorm = instructionVectorFuncVectorToPrim float vectorFloat twoNorm

-- |Takes the cummulative mean of the float vector, rounds the results and places them floato a vector as the caluculations happen and pushes it back to the top of
-- the float vector stack.
instructionVectorFloatCummulativeMean :: State -> State
instructionVectorFloatCummulativeMean = instructionVectorFuncVectorToVector vectorFloat (\xs -> zipWith (/) (scanl1 (+) xs) [1..])

-- |Takes the cummulative sum of the float vector, places the results in a vector as the caluculations happen and pushes it back to the top of
-- the float vector stack.
instructionVectorFloatCummulativeSum :: State -> State
instructionVectorFloatCummulativeSum = instructionVectorFuncVectorToVector vectorFloat (scanl1 (+))

-- |Takes the cummulative max of the float vector, places the results in a vector as the caluculations happen and pushes it back to the top of
-- the float vector stack.
instructionVectorFloatCummulativeMax :: State -> State
instructionVectorFloatCummulativeMax = instructionVectorFuncVectorToVector vectorFloat (scanl1 max)

-- |Takes the cummulative min of the float vector, places the results in a vector as the caluculations happen and pushes it back to the top of
-- the float vector stack.
instructionVectorFloatCummulativeMin :: State -> State
instructionVectorFloatCummulativeMin = instructionVectorFuncVectorToVector vectorFloat (scanl1 min)

-- |Applies the exponential function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatExp :: State -> State
instructionVectorFloatExp = instructionVectorFuncVectorToVector vectorFloat (map exp)

-- |Applies the log function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatLog :: State -> State
instructionVectorFloatLog = instructionVectorFuncVectorToVector vectorFloat (map log)

-- |Applies the sin function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatSin :: State -> State
instructionVectorFloatSin = instructionVectorFuncVectorToVector vectorFloat (map sin)

-- |Applies the cos function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatCos :: State -> State
instructionVectorFloatCos = instructionVectorFuncVectorToVector vectorFloat (map cos)

-- |Applies the tan function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatTan :: State -> State
instructionVectorFloatTan = instructionVectorFuncVectorToVector vectorFloat (map tan)

-- |Applies the abs function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatAbs :: State -> State
instructionVectorFloatAbs = instructionVectorFuncVectorToVector vectorFloat (map abs)

-- |Applies the square function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatSquare :: State -> State
instructionVectorFloatSquare = instructionVectorFuncVectorToVector vectorFloat (map (^ 2))

-- |Applies the cube function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatCube :: State -> State
instructionVectorFloatCube = instructionVectorFuncVectorToVector vectorFloat (map (^ 3))

-- |Applies the sqrt function to all indices in an float vector, rounds the result as it moves along.
instructionVectorFloatSqrt :: State -> State
instructionVectorFloatSqrt = instructionVectorFuncVectorToVector vectorFloat (map sqrt)

allVectorFloatInstructions :: [Gene]
allVectorFloatInstructions = map StateFunc ($(functionExtractor "instruction"))
