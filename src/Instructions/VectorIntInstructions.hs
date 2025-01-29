module Instructions.VectorIntInstructions where

import Instructions.GenericInstructions
import State

instructionIntVectorConcat :: State -> State
instructionIntVectorConcat state = instructionConcat state intVector

instructionIntVectorConj :: State -> State
instructionIntVectorConj state = instructionConj state int intVector

instructionIntVectorTakeN :: State -> State
instructionIntVectorTakeN state = instructionTakeN state intVector

instructionIntVectorSubVector :: State -> State
instructionIntVectorSubVector state = instructionSubVector state intVector

instructionIntVectorFirst :: State -> State
instructionIntVectorFirst state = instructionVectorFirst state int intVector

instructionIntVectorLast :: State -> State
instructionIntVectorLast state = instructionVectorLast state int intVector

instructionIntVectorNth :: State -> State
instructionIntVectorNth state = instructionVectorNth state int intVector

instructionIntVectorRest :: State -> State
instructionIntVectorRest state = instructionRest state intVector

instructionIntVectorButLast :: State -> State
instructionIntVectorButLast state = instructionButLast state intVector

instructionIntVectorLength :: State -> State
instructionIntVectorLength state = instructionLength state intVector

instructionIntVectorReverse :: State -> State
instructionIntVectorReverse state = instructionReverse state intVector

instructionIntVectorPushAll :: State -> State
instructionIntVectorPushAll state = instructionPushAll state int intVector

instructionIntVectorMakeEmpty :: State -> State
instructionIntVectorMakeEmpty state = instructionVectorMakeEmpty state intVector

instructionIntVectorIsEmpty :: State -> State
instructionIntVectorIsEmpty state = instructionVectorIsEmpty state intVector

instructionIntVectorIndexOf :: State -> State
instructionIntVectorIndexOf state = instructionVectorIndexOf state int intVector

instructionIntVectorOccurrencesOf :: State -> State
instructionIntVectorOccurrencesOf state = instructionVectorOccurrencesOf state int intVector

instructionIntVectorSetNth :: State -> State
instructionIntVectorSetNth state = instructionVectorSetNth state int intVector
