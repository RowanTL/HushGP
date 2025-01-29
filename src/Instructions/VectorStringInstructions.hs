module Instructions.VectorStringInstructions where

import State
import Instructions.GenericInstructions

instructionVectorStringConcat :: State -> State
instructionVectorStringConcat state = instructionConcat state vectorString

instructionVectorStringConj :: State -> State
instructionVectorStringConj state = instructionConj state string vectorString

instructionVectorStringTakeN :: State -> State
instructionVectorStringTakeN state = instructionTakeN state vectorString

instructionVectorStringSubVector :: State -> State
instructionVectorStringSubVector state = instructionSubVector state vectorString

instructionVectorStringFirst :: State -> State
instructionVectorStringFirst state = instructionVectorFirst state string vectorString

instructionVectorStringLast :: State -> State
instructionVectorStringLast state = instructionVectorLast state string vectorString

instructionVectorStringNth :: State -> State
instructionVectorStringNth state = instructionVectorNth state string vectorString

instructionVectorStringRest :: State -> State
instructionVectorStringRest state = instructionRest state vectorString

instructionVectorStringButLast :: State -> State
instructionVectorStringButLast state = instructionButLast state vectorString

instructionVectorStringLength :: State -> State
instructionVectorStringLength state = instructionLength state vectorString

instructionVectorStringReverse :: State -> State
instructionVectorStringReverse state = instructionReverse state vectorString

instructionVectorStringPushAll :: State -> State
instructionVectorStringPushAll state = instructionPushAll state string vectorString

instructionVectorStringMakeEmpty :: State -> State
instructionVectorStringMakeEmpty state = instructionVectorMakeEmpty state vectorString

instructionVectorStringIsEmpty :: State -> State
instructionVectorStringIsEmpty state = instructionVectorIsEmpty state vectorString

instructionVectorStringIndexOf :: State -> State
instructionVectorStringIndexOf state = instructionVectorIndexOf state string vectorString

instructionVectorStringOccurrencesOf :: State -> State
instructionVectorStringOccurrencesOf state = instructionVectorOccurrencesOf state string vectorString

instructionVectorStringSetNth :: State -> State
instructionVectorStringSetNth state = instructionVectorSetNth state string vectorString

instructionVectorStringReplace :: State -> State
instructionVectorStringReplace state = instructionVectorReplace state string vectorString

instructionVectorStringReplaceFirst :: State -> State
instructionVectorStringReplaceFirst state = instructionVectorReplaceFirst state string vectorString

instructionVectorStringRemove :: State -> State
instructionVectorStringRemove state = instructionVectorRemove state string vectorString

instructionVectorStringIterate :: State -> State
instructionVectorStringIterate state = instructionVectorIterate state string vectorString GeneVectorString instructionVectorStringIterate
