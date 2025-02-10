module HushGP.Instructions.VectorBoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

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

instructionVectorBoolYank :: State -> State
instructionVectorBoolYank = instructionYank vectorBool

instructionVectorBoolYankDup :: State -> State
instructionVectorBoolYankDup = instructionYankDup vectorBool

instructionVectorBoolIsStackEmpty :: State -> State
instructionVectorBoolIsStackEmpty = instructionIsStackEmpty vectorBool

instructionVectorBoolShove :: State -> State
instructionVectorBoolShove = instructionShove vectorBool

instructionVectorBoolShoveDup :: State -> State
instructionVectorBoolShoveDup = instructionShoveDup vectorBool

instructionVectorBoolDupItems :: State -> State
instructionVectorBoolDupItems = instructionDupItems vectorBool

instructionVectorBoolConcat :: State -> State
instructionVectorBoolConcat = instructionVectorConcat vectorBool

instructionVectorBoolConj :: State -> State
instructionVectorBoolConj = instructionVectorConj bool vectorBool

instructionVectorBoolConjEnd :: State -> State
instructionVectorBoolConjEnd = instructionVectorConjEnd bool vectorBool

instructionVectorBoolTakeN :: State -> State
instructionVectorBoolTakeN = instructionVectorTakeN vectorBool

instructionVectorBoolTakeRN :: State -> State
instructionVectorBoolTakeRN = instructionVectorTakeRN vectorBool

instructionVectorBoolSubVector :: State -> State
instructionVectorBoolSubVector = instructionSubVector vectorBool

instructionVectorBoolFirst :: State -> State
instructionVectorBoolFirst = instructionVectorFirst bool vectorBool

instructionVectorBoolFromFirstPrim :: State -> State
instructionVectorBoolFromFirstPrim = instructionVectorFromFirstPrim vectorBool

instructionVectorBoolFromPrim :: State -> State
instructionVectorBoolFromPrim = instructionVectorFromPrim bool vectorBool

instructionVectorBoolLast :: State -> State
instructionVectorBoolLast = instructionVectorLast bool vectorBool

instructionVectorBoolFromLastPrim :: State -> State
instructionVectorBoolFromLastPrim = instructionVectorFromLastPrim vectorBool

instructionVectorBoolNth :: State -> State
instructionVectorBoolNth = instructionVectorNth bool vectorBool

instructionVectorBoolFromNthPrim :: State -> State
instructionVectorBoolFromNthPrim = instructionVectorFromNthPrim vectorBool

instructionVectorBoolRest :: State -> State
instructionVectorBoolRest = instructionVectorRest vectorBool

instructionVectorBoolButLast :: State -> State
instructionVectorBoolButLast = instructionVectorButLast vectorBool

instructionVectorBoolDrop :: State -> State
instructionVectorBoolDrop = instructionVectorDrop vectorBool

instructionVectorBoolDropR :: State -> State
instructionVectorBoolDropR = instructionVectorDropR vectorBool

instructionVectorBoolLength :: State -> State
instructionVectorBoolLength = instructionLength vectorBool

instructionVectorBoolReverse :: State -> State
instructionVectorBoolReverse = instructionReverse vectorBool

instructionVectorBoolPushAll :: State -> State
instructionVectorBoolPushAll = instructionPushAll bool vectorBool

instructionVectorBoolMakeEmpty :: State -> State
instructionVectorBoolMakeEmpty = instructionVectorMakeEmpty vectorBool

instructionVectorBoolIsEmpty :: State -> State
instructionVectorBoolIsEmpty = instructionVectorIsEmpty vectorBool

instructionVectorBoolContains :: State -> State
instructionVectorBoolContains = instructionVectorContains bool vectorBool

instructionVectorBoolContainsVectorBool :: State -> State
instructionVectorBoolContainsVectorBool = instructionVectorContainsVector vectorBool

instructionVectorBoolIndexOf :: State -> State
instructionVectorBoolIndexOf = instructionVectorIndexOf bool vectorBool

instructionVectorBoolIndexOfVectorBool :: State -> State
instructionVectorBoolIndexOfVectorBool = instructionVectorIndexOfVector vectorBool

instructionVectorBoolOccurrencesOf :: State -> State
instructionVectorBoolOccurrencesOf = instructionVectorOccurrencesOf bool vectorBool

instructionVectorBoolOccurrencesOfVectorBool :: State -> State
instructionVectorBoolOccurrencesOfVectorBool = instructionVectorOccurrencesOfVector vectorBool

instructionVectorBoolParseToBool :: State -> State
instructionVectorBoolParseToBool = instructionVectorParseToPrim vectorBool

instructionVectorBoolSetNth :: State -> State
instructionVectorBoolSetNth = instructionVectorSetNth bool vectorBool

instructionVectorBoolSplitOn :: State -> State
instructionVectorBoolSplitOn = instructionVectorSplitOn bool vectorBool

instructionVectorBoolSplitOnVectorBool :: State -> State
instructionVectorBoolSplitOnVectorBool = instructionVectorSplitOnVector vectorBool

instructionVectorBoolReplaceFirst :: State -> State
instructionVectorBoolReplaceFirst = instructionVectorReplace bool vectorBool (Just 1)

instructionVectorBoolReplaceAll :: State -> State
instructionVectorBoolReplaceAll = instructionVectorReplace bool vectorBool Nothing

instructionVectorBoolReplaceN :: State -> State
instructionVectorBoolReplaceN = instructionVectorReplaceN bool vectorBool

instructionVectorBoolReplaceFirstVectorBool :: State -> State
instructionVectorBoolReplaceFirstVectorBool = instructionVectorReplaceVector vectorBool (Just 1)

instructionVectorBoolReplaceAllVectorBool :: State -> State
instructionVectorBoolReplaceAllVectorBool = instructionVectorReplaceVector vectorBool Nothing

instructionVectorBoolReplaceVectorBoolN :: State -> State
instructionVectorBoolReplaceVectorBoolN = instructionVectorReplaceVectorN vectorBool

instructionVectorBoolRemoveFirst :: State -> State
instructionVectorBoolRemoveFirst = instructionVectorRemove bool vectorBool (Just 1)

instructionVectorBoolRemoveAll :: State -> State
instructionVectorBoolRemoveAll = instructionVectorRemove bool vectorBool Nothing

instructionVectorBoolRemoveN :: State -> State
instructionVectorBoolRemoveN = instructionVectorRemoveN bool vectorBool

instructionVectorBoolRemoveFirstVectorBool :: State -> State
instructionVectorBoolRemoveFirstVectorBool = instructionVectorRemoveVector vectorBool (Just 1)

instructionVectorBoolRemoveAllVectorBool :: State -> State
instructionVectorBoolRemoveAllVectorBool = instructionVectorRemoveVector vectorBool Nothing

instructionVectorBoolRemoveNVectorBool :: State -> State
instructionVectorBoolRemoveNVectorBool = instructionVectorRemoveVectorN vectorBool

instructionVectorBoolIterate :: State -> State
instructionVectorBoolIterate = instructionVectorIterate bool vectorBool GeneVectorBool instructionVectorBoolIterate "instructionVectorBoolIterate"

instructionVectorBoolSort :: State -> State
instructionVectorBoolSort = instructionVectorSort vectorBool

instructionVectorBoolSortReverse :: State -> State
instructionVectorBoolSortReverse = instructionVectorSortReverse vectorBool

instructionVectorBoolInsert :: State -> State
instructionVectorBoolInsert = instructionVectorInsert bool vectorBool

instructionVectorBoolInsertVectorBool :: State -> State
instructionVectorBoolInsertVectorBool = instructionVectorInsertVector vectorBool
