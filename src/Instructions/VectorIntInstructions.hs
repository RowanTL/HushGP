module Instructions.VectorIntInstructions where

import Instructions.GenericInstructions
import State

instructionVectorIntConcat :: State -> State
instructionVectorIntConcat state = instructionConcat state vectorInt

instructionVectorIntConj :: State -> State
instructionVectorIntConj state = instructionConj state int vectorInt

instructionVectorIntTakeN :: State -> State
instructionVectorIntTakeN state = instructionTakeN state vectorInt

instructionVectorIntSubVector :: State -> State
instructionVectorIntSubVector state = instructionSubVector state vectorInt

instructionVectorIntFirst :: State -> State
instructionVectorIntFirst state = instructionVectorFirst state int vectorInt

instructionVectorIntLast :: State -> State
instructionVectorIntLast state = instructionVectorLast state int vectorInt

instructionVectorIntNth :: State -> State
instructionVectorIntNth state = instructionVectorNth state int vectorInt

instructionVectorIntRest :: State -> State
instructionVectorIntRest state = instructionRest state vectorInt

instructionVectorIntButLast :: State -> State
instructionVectorIntButLast state = instructionButLast state vectorInt

instructionVectorIntLength :: State -> State
instructionVectorIntLength state = instructionLength state vectorInt

instructionVectorIntReverse :: State -> State
instructionVectorIntReverse state = instructionReverse state vectorInt

instructionVectorIntPushAll :: State -> State
instructionVectorIntPushAll state = instructionPushAll state int vectorInt

instructionVectorIntMakeEmpty :: State -> State
instructionVectorIntMakeEmpty state = instructionVectorMakeEmpty state vectorInt

instructionVectorIntIsEmpty :: State -> State
instructionVectorIntIsEmpty state = instructionVectorIsEmpty state vectorInt

instructionVectorIntIndexOf :: State -> State
instructionVectorIntIndexOf state = instructionVectorIndexOf state int vectorInt

instructionVectorIntOccurrencesOf :: State -> State
instructionVectorIntOccurrencesOf state = instructionVectorOccurrencesOf state int vectorInt

instructionVectorIntSetNth :: State -> State
instructionVectorIntSetNth state = instructionVectorSetNth state int vectorInt

instructionVectorIntReplace :: State -> State
instructionVectorIntReplace state = instructionVectorReplace state int vectorInt

instructionVectorIntReplaceFirst :: State -> State
instructionVectorIntReplaceFirst state = instructionVectorReplaceFirst state int vectorInt

instructionVectorIntRemove :: State -> State
instructionVectorIntRemove state = instructionVectorRemove state int vectorInt

instructionVectorIntIterate :: State -> State
instructionVectorIntIterate state = instructionVectorIterate state int vectorInt GeneVectorInt instructionVectorIntIterate
