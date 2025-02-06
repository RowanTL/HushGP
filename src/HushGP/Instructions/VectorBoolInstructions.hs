module HushGP.Instructions.VectorBoolInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorBoolConcat :: State -> State
instructionVectorBoolConcat state = instructionConcat state vectorBool

instructionVectorBoolConj :: State -> State
instructionVectorBoolConj state = instructionConj state bool vectorBool

instructionVectorBoolTakeN :: State -> State
instructionVectorBoolTakeN state = instructionTakeN state vectorBool

instructionVectorBoolSubVector :: State -> State
instructionVectorBoolSubVector state = instructionSubVector state vectorBool

instructionVectorBoolFirst :: State -> State
instructionVectorBoolFirst state = instructionVectorFirst state bool vectorBool

instructionVectorBoolLast :: State -> State
instructionVectorBoolLast state = instructionVectorLast state bool vectorBool

instructionVectorBoolNth :: State -> State
instructionVectorBoolNth state = instructionVectorNth state bool vectorBool

instructionVectorBoolRest :: State -> State
instructionVectorBoolRest state = instructionRest state vectorBool

instructionVectorBoolButLast :: State -> State
instructionVectorBoolButLast state = instructionButLast state vectorBool

instructionVectorBoolLength :: State -> State
instructionVectorBoolLength state = instructionLength state vectorBool

instructionVectorBoolReverse :: State -> State
instructionVectorBoolReverse state = instructionReverse state vectorBool

instructionVectorBoolPushAll :: State -> State
instructionVectorBoolPushAll state = instructionPushAll state bool vectorBool

instructionVectorBoolMakeEmpty :: State -> State
instructionVectorBoolMakeEmpty state = instructionVectorMakeEmpty state vectorBool

instructionVectorBoolIsEmpty :: State -> State
instructionVectorBoolIsEmpty state = instructionVectorIsEmpty state vectorBool

instructionVectorBoolIndexOf :: State -> State
instructionVectorBoolIndexOf state = instructionVectorIndexOf state bool vectorBool

instructionVectorBoolOccurrencesOf :: State -> State
instructionVectorBoolOccurrencesOf state = instructionVectorOccurrencesOf state bool vectorBool

instructionVectorBoolSetNth :: State -> State
instructionVectorBoolSetNth state = instructionVectorSetNth state bool vectorBool

instructionVectorBoolReplace :: State -> State
instructionVectorBoolReplace state = instructionVectorReplace state bool vectorBool

instructionVectorBoolReplaceFirst :: State -> State
instructionVectorBoolReplaceFirst state = instructionVectorReplaceFirst state bool vectorBool

instructionVectorBoolRemove :: State -> State
instructionVectorBoolRemove state = instructionVectorRemove state bool vectorBool

instructionVectorBoolIterate :: State -> State
instructionVectorBoolIterate state = instructionVectorIterate state bool vectorBool GeneVectorBool instructionVectorBoolIterate "instructionVectorBoolIterate"

instructionVectorBoolPop :: State -> State
instructionVectorBoolPop state = instructionPop state vectorBool

instructionVectorBoolDup :: State -> State
instructionVectorBoolDup state = instructionDup state vectorBool

instructionVectorBoolDupN :: State -> State
instructionVectorBoolDupN state = instructionDupN state vectorBool

instructionVectorBoolSwap :: State -> State
instructionVectorBoolSwap state = instructionSwap state vectorBool

instructionVectorBoolRot :: State -> State
instructionVectorBoolRot state = instructionRot state vectorBool

instructionVectorBoolFlush :: State -> State
instructionVectorBoolFlush state = instructionFlush state vectorBool

instructionVectorBoolEq :: State -> State
instructionVectorBoolEq state = instructionEq state vectorBool

instructionVectorBoolStackDepth :: State -> State
instructionVectorBoolStackDepth state = instructionStackDepth state vectorBool

instructionVectorBoolYank :: State -> State
instructionVectorBoolYank state = instructionYank state vectorBool

instructionVectorBoolYankDup :: State -> State
instructionVectorBoolYankDup state = instructionYankDup state vectorBool

instructionVectorBoolStackIsEmpty :: State -> State
instructionVectorBoolStackIsEmpty state = instructionIsEmpty state vectorBool

instructionVectorBoolShove :: State -> State
instructionVectorBoolShove state = instructionShove state vectorBool

instructionVectorBoolShoveDup :: State -> State
instructionVectorBoolShoveDup state = instructionShoveDup state vectorBool
