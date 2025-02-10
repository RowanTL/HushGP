module HushGP.Instructions.VectorStringInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorStringConcat :: State -> State
instructionVectorStringConcat = instructionVectorConcat vectorString

instructionVectorStringConj :: State -> State
instructionVectorStringConj = instructionVectorConj string vectorString

instructionVectorStringTakeN :: State -> State
instructionVectorStringTakeN = instructionVectorTakeN vectorString

instructionVectorStringSubVector :: State -> State
instructionVectorStringSubVector = instructionSubVector vectorString

instructionVectorStringFirst :: State -> State
instructionVectorStringFirst = instructionVectorFirst string vectorString

instructionVectorStringLast :: State -> State
instructionVectorStringLast = instructionVectorLast string vectorString

instructionVectorStringNth :: State -> State
instructionVectorStringNth = instructionVectorNth string vectorString

instructionVectorStringRest :: State -> State
instructionVectorStringRest = instructionVectorRest vectorString

instructionVectorStringButLast :: State -> State
instructionVectorStringButLast = instructionVectorButLast vectorString

instructionVectorStringLength :: State -> State
instructionVectorStringLength = instructionLength vectorString

instructionVectorStringReverse :: State -> State
instructionVectorStringReverse = instructionReverse vectorString

instructionVectorStringPushAll :: State -> State
instructionVectorStringPushAll = instructionPushAll string vectorString

instructionVectorStringMakeEmpty :: State -> State
instructionVectorStringMakeEmpty = instructionVectorMakeEmpty vectorString

instructionVectorStringIsEmpty :: State -> State
instructionVectorStringIsEmpty = instructionVectorIsEmpty vectorString

instructionVectorStringIndexOf :: State -> State
instructionVectorStringIndexOf = instructionVectorIndexOf string vectorString

instructionVectorStringOccurrencesOf :: State -> State
instructionVectorStringOccurrencesOf = instructionVectorOccurrencesOf string vectorString

instructionVectorStringSetNth :: State -> State
instructionVectorStringSetNth = instructionVectorSetNth string vectorString

instructionVectorStringReplace :: State -> State
instructionVectorStringReplace = instructionVectorReplace string vectorString Nothing

instructionVectorStringReplaceFirst :: State -> State
instructionVectorStringReplaceFirst = instructionVectorReplace string vectorString (Just 1)

instructionVectorStringRemove :: State -> State
instructionVectorStringRemove = instructionVectorRemove string vectorString Nothing

instructionVectorStringIterate :: State -> State
instructionVectorStringIterate = instructionVectorIterate string vectorString GeneVectorString instructionVectorStringIterate "instructionVectorStringIterate"

instructionVectorStringPop :: State -> State
instructionVectorStringPop = instructionPop vectorString

instructionVectorStringDup :: State -> State
instructionVectorStringDup = instructionDup vectorString

instructionVectorStringDupN :: State -> State
instructionVectorStringDupN = instructionDupN vectorString

instructionVectorStringSwap :: State -> State
instructionVectorStringSwap = instructionSwap vectorString

instructionVectorStringRot :: State -> State
instructionVectorStringRot = instructionRot vectorString

instructionVectorStringFlush :: State -> State
instructionVectorStringFlush = instructionFlush vectorString

instructionVectorStringEq :: State -> State
instructionVectorStringEq = instructionEq vectorString

instructionVectorStringStackDepth :: State -> State
instructionVectorStringStackDepth = instructionStackDepth vectorString

instructionVectorStringYank :: State -> State
instructionVectorStringYank = instructionYank vectorString

instructionVectorStringYankDup :: State -> State
instructionVectorStringYankDup = instructionYankDup vectorString

instructionVectorStringIsStackEmpty :: State -> State
instructionVectorStringIsStackEmpty = instructionIsStackEmpty vectorString

instructionVectorStringShove :: State -> State
instructionVectorStringShove = instructionShove vectorString

instructionVectorStringShoveDup :: State -> State
instructionVectorStringShoveDup = instructionShoveDup vectorString

instructionVectorStringSort :: State -> State
instructionVectorStringSort = instructionVectorSort vectorString

instructionVectorStringSortReverse :: State -> State
instructionVectorStringSortReverse = instructionVectorSortReverse vectorString

instructionVectorStringDupItems :: State -> State
instructionVectorStringDupItems = instructionDupItems vectorString
