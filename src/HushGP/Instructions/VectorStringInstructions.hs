module HushGP.Instructions.VectorStringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

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
instructionVectorStringIterate state = instructionVectorIterate state string vectorString GeneVectorString instructionVectorStringIterate "instructionVectorStringIterate"

instructionVectorStringPop :: State -> State
instructionVectorStringPop state = instructionPop state vectorString

instructionVectorStringDup :: State -> State
instructionVectorStringDup state = instructionDup state vectorString

instructionVectorStringDupN :: State -> State
instructionVectorStringDupN state = instructionDupN state vectorString

instructionVectorStringSwap :: State -> State
instructionVectorStringSwap state = instructionSwap state vectorString

instructionVectorStringRot :: State -> State
instructionVectorStringRot state = instructionRot state vectorString

instructionVectorStringFlush :: State -> State
instructionVectorStringFlush state = instructionFlush state vectorString

instructionVectorStringEq :: State -> State
instructionVectorStringEq state = instructionEq state vectorString

instructionVectorStringStackDepth :: State -> State
instructionVectorStringStackDepth state = instructionStackDepth state vectorString

instructionVectorStringYank :: State -> State
instructionVectorStringYank state = instructionYank state vectorString

instructionVectorStringYankDup :: State -> State
instructionVectorStringYankDup state = instructionYankDup state vectorString

instructionVectorStringIsStackEmpty :: State -> State
instructionVectorStringIsStackEmpty state = instructionIsStackEmpty state vectorString

instructionVectorStringShove :: State -> State
instructionVectorStringShove state = instructionShove state vectorString

instructionVectorStringShoveDup :: State -> State
instructionVectorStringShoveDup state = instructionShoveDup state vectorString

instructionVectorStringSort :: State -> State
instructionVectorStringSort = instructionVectorSort vectorString

instructionVectorStringSortReverse :: State -> State
instructionVectorStringSortReverse = instructionVectorSortReverse vectorString

instructionVectorStringDupItems :: State -> State
instructionVectorStringDupItems = instructionDupItems vectorString
