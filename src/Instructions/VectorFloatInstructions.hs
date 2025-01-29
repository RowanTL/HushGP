module Instructions.VectorFloatInstructions where

import State
import Instructions.GenericInstructions

instructionVectorFloatConcat :: State -> State
instructionVectorFloatConcat state = instructionConcat state vectorFloat

instructionVectorFloatConj :: State -> State
instructionVectorFloatConj state = instructionConj state float vectorFloat

instructionVectorFloatTakeN :: State -> State
instructionVectorFloatTakeN state = instructionTakeN state vectorInt

instructionVectorFloatSubVector :: State -> State
instructionVectorFloatSubVector state = instructionSubVector state vectorInt

instructionVectorFloatFirst :: State -> State
instructionVectorFloatFirst state = instructionVectorFirst state float vectorFloat

instructionVectorFloatLast :: State -> State
instructionVectorFloatLast state = instructionVectorLast state float vectorFloat

instructionVectorFloatNth :: State -> State
instructionVectorFloatNth state = instructionVectorNth state float vectorFloat

instructionVectorFloatRest :: State -> State
instructionVectorFloatRest state = instructionRest state vectorFloat

instructionVectorFloatButLast :: State -> State
instructionVectorFloatButLast state = instructionButLast state vectorFloat

instructionVectorFloatLength :: State -> State
instructionVectorFloatLength state = instructionLength state vectorFloat

instructionVectorFloatReverse :: State -> State
instructionVectorFloatReverse state = instructionReverse state vectorFloat

instructionVectorFloatPushAll :: State -> State
instructionVectorFloatPushAll state = instructionPushAll state float vectorFloat

instructionVectorFloatMakeEmpty :: State -> State
instructionVectorFloatMakeEmpty state = instructionVectorMakeEmpty state vectorFloat

instructionVectorFloatIsEmpty :: State -> State
instructionVectorFloatIsEmpty state = instructionVectorIsEmpty state vectorFloat

instructionVectorFloatIndexOf :: State -> State
instructionVectorFloatIndexOf state = instructionVectorIndexOf state float vectorFloat

instructionVectorFloatOccurrencesOf :: State -> State
instructionVectorFloatOccurrencesOf state = instructionVectorOccurrencesOf state float vectorFloat

instructionVectorFloatSetNth :: State -> State
instructionVectorFloatSetNth state = instructionVectorSetNth state float vectorFloat

instructionVectorFloatReplace :: State -> State
instructionVectorFloatReplace state = instructionVectorReplace state float vectorFloat

instructionVectorFloatReplaceFirst :: State -> State
instructionVectorFloatReplaceFirst state = instructionVectorReplaceFirst state float vectorFloat

instructionVectorFloatRemove :: State -> State
instructionVectorFloatRemove state = instructionVectorRemove state float vectorFloat
