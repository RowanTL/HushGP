module HushGP.Instructions.VectorBoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorBoolConcat :: State -> State
instructionVectorBoolConcat = instructionVectorConcat vectorBool

instructionVectorBoolConj :: State -> State
instructionVectorBoolConj = instructionVectorConj bool vectorBool

instructionVectorBoolTakeN :: State -> State
instructionVectorBoolTakeN = instructionVectorTakeN vectorBool

instructionVectorBoolSubVector :: State -> State
instructionVectorBoolSubVector = instructionSubVector vectorBool

instructionVectorBoolFirst :: State -> State
instructionVectorBoolFirst = instructionVectorFirst bool vectorBool

instructionVectorBoolLast :: State -> State
instructionVectorBoolLast = instructionVectorLast bool vectorBool

instructionVectorBoolNth :: State -> State
instructionVectorBoolNth = instructionVectorNth bool vectorBool

instructionVectorBoolRest :: State -> State
instructionVectorBoolRest = instructionVectorRest vectorBool

instructionVectorBoolButLast :: State -> State
instructionVectorBoolButLast = instructionVectorButLast vectorBool

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

instructionVectorBoolIndexOf :: State -> State
instructionVectorBoolIndexOf = instructionVectorIndexOf bool vectorBool

instructionVectorBoolOccurrencesOf :: State -> State
instructionVectorBoolOccurrencesOf = instructionVectorOccurrencesOf bool vectorBool

instructionVectorBoolSetNth :: State -> State
instructionVectorBoolSetNth = instructionVectorSetNth bool vectorBool

instructionVectorBoolReplace :: State -> State
instructionVectorBoolReplace = instructionVectorReplace bool vectorBool Nothing

instructionVectorBoolReplaceFirst :: State -> State
instructionVectorBoolReplaceFirst = instructionVectorReplace bool vectorBool (Just 1)

instructionVectorBoolRemove :: State -> State
instructionVectorBoolRemove = instructionVectorRemove bool vectorBool Nothing

instructionVectorBoolIterate :: State -> State
instructionVectorBoolIterate = instructionVectorIterate bool vectorBool GeneVectorBool instructionVectorBoolIterate "instructionVectorBoolIterate"

instructionVectorBoolPop :: State -> State
instructionVectorBoolPop = instructionPop vectorBool

instructionVectorBoolDup :: State -> State
instructionVectorBoolDup = instructionDup vectorBool

instructionVectorBoolDupN :: State -> State
instructionVectorBoolDupN = instructionDupN vectorBool

instructionVectorBoolSwap :: State -> State
instructionVectorBoolSwap = instructionSwap vectorBool

instructionVectorBoolRot :: State -> State
instructionVectorBoolRot = instructionRot vectorBool

instructionVectorBoolFlush :: State -> State
instructionVectorBoolFlush = instructionFlush vectorBool

instructionVectorBoolEq :: State -> State
instructionVectorBoolEq = instructionEq vectorBool

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

instructionVectorBoolSort :: State -> State
instructionVectorBoolSort = instructionVectorSort vectorBool

instructionVectorBoolSortReverse :: State -> State
instructionVectorBoolSortReverse = instructionVectorSortReverse vectorBool

instructionVectorBoolDupItems :: State -> State
instructionVectorBoolDupItems = instructionDupItems vectorBool
