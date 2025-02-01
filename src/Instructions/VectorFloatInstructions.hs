module Instructions.VectorFloatInstructions where

import State
import Instructions.GenericInstructions

instructionVectorFloatConcat :: State -> State
instructionVectorFloatConcat state = instructionConcat state vectorFloat

instructionVectorFloatConj :: State -> State
instructionVectorFloatConj state = instructionConj state float vectorFloat

instructionVectorFloatTakeN :: State -> State
instructionVectorFloatTakeN state = instructionTakeN state vectorFloat

instructionVectorFloatSubVector :: State -> State
instructionVectorFloatSubVector state = instructionSubVector state vectorFloat

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

instructionVectorFloatIterate :: State -> State
instructionVectorFloatIterate state = instructionVectorIterate state float vectorFloat GeneVectorFloat instructionVectorFloatIterate "instructionVectorFloatIterate"

instructionVectorFloatPop :: State -> State
instructionVectorFloatPop state = instructionPop state vectorFloat

instructionVectorFloatDup :: State -> State
instructionVectorFloatDup state = instructionDup state vectorFloat

instructionVectorFloatDupN :: State -> State
instructionVectorFloatDupN state = instructionDupN state vectorFloat

instructionVectorFloatSwap :: State -> State
instructionVectorFloatSwap state = instructionSwap state vectorFloat

instructionVectorFloatRot :: State -> State
instructionVectorFloatRot state = instructionRot state vectorFloat

instructionVectorFloatFlush :: State -> State
instructionVectorFloatFlush state = instructionFlush state vectorFloat

instructionVectorFloatEq :: State -> State
instructionVectorFloatEq state = instructionEq state vectorFloat

instructionVectorFloatStackDepth :: State -> State
instructionVectorFloatStackDepth state = instructionStackDepth state vectorFloat

instructionVectorFloatYank :: State -> State
instructionVectorFloatYank state = instructionYank state vectorFloat

instructionVectorFloatYankDup :: State -> State
instructionVectorFloatYankDup state = instructionYankDup state vectorFloat

instructionVectorFloatStackIsEmpty :: State -> State
instructionVectorFloatStackIsEmpty state = instructionIsEmpty state vectorFloat

instructionVectorFloatShove :: State -> State
instructionVectorFloatShove state = instructionShove state vectorFloat

instructionVectorFloatShoveDup :: State -> State
instructionVectorFloatShoveDup state = instructionShoveDup state vectorFloat
