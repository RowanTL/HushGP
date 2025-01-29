module Instructions.VectorLogicalInstructions where

import State
import Instructions.GenericInstructions

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
instructionVectorBoolIterate state = instructionVectorIterate state bool vectorBool GeneVectorBool instructionVectorBoolIterate
