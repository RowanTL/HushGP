module HushGP.Instructions.VectorFloatInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorFloatConcat :: State -> State
instructionVectorFloatConcat = instructionVectorConcat vectorFloat

instructionVectorFloatConj :: State -> State
instructionVectorFloatConj = instructionVectorConj float vectorFloat

instructionVectorFloatTakeN :: State -> State
instructionVectorFloatTakeN = instructionVectorTakeN vectorFloat

instructionVectorFloatSubVector :: State -> State
instructionVectorFloatSubVector = instructionSubVector vectorFloat

instructionVectorFloatFirst :: State -> State
instructionVectorFloatFirst = instructionVectorFirst float vectorFloat

instructionVectorFloatLast :: State -> State
instructionVectorFloatLast = instructionVectorLast float vectorFloat

instructionVectorFloatNth :: State -> State
instructionVectorFloatNth = instructionVectorNth float vectorFloat

instructionVectorFloatRest :: State -> State
instructionVectorFloatRest = instructionVectorRest vectorFloat

instructionVectorFloatButLast :: State -> State
instructionVectorFloatButLast = instructionVectorButLast vectorFloat

instructionVectorFloatLength :: State -> State
instructionVectorFloatLength = instructionLength vectorFloat

instructionVectorFloatReverse :: State -> State
instructionVectorFloatReverse = instructionReverse vectorFloat

instructionVectorFloatPushAll :: State -> State
instructionVectorFloatPushAll = instructionPushAll float vectorFloat

instructionVectorFloatMakeEmpty :: State -> State
instructionVectorFloatMakeEmpty = instructionVectorMakeEmpty vectorFloat

instructionVectorFloatIsEmpty :: State -> State
instructionVectorFloatIsEmpty = instructionVectorIsEmpty vectorFloat

instructionVectorFloatIndexOf :: State -> State
instructionVectorFloatIndexOf = instructionVectorIndexOf float vectorFloat

instructionVectorFloatOccurrencesOf :: State -> State
instructionVectorFloatOccurrencesOf = instructionVectorOccurrencesOf float vectorFloat

instructionVectorFloatSetNth :: State -> State
instructionVectorFloatSetNth = instructionVectorSetNth float vectorFloat

instructionVectorFloatReplace :: State -> State
instructionVectorFloatReplace = instructionVectorReplace float vectorFloat Nothing

instructionVectorFloatReplaceFirst :: State -> State
instructionVectorFloatReplaceFirst = instructionVectorReplace float vectorFloat (Just 1)

instructionVectorFloatRemove :: State -> State
instructionVectorFloatRemove = instructionVectorRemove float vectorFloat Nothing

instructionVectorFloatIterate :: State -> State
instructionVectorFloatIterate = instructionVectorIterate float vectorFloat GeneVectorFloat instructionVectorFloatIterate "instructionVectorFloatIterate"

instructionVectorFloatPop :: State -> State
instructionVectorFloatPop = instructionPop vectorFloat

instructionVectorFloatDup :: State -> State
instructionVectorFloatDup = instructionDup vectorFloat

instructionVectorFloatDupN :: State -> State
instructionVectorFloatDupN = instructionDupN vectorFloat

instructionVectorFloatSwap :: State -> State
instructionVectorFloatSwap = instructionSwap vectorFloat

instructionVectorFloatRot :: State -> State
instructionVectorFloatRot = instructionRot vectorFloat

instructionVectorFloatFlush :: State -> State
instructionVectorFloatFlush = instructionFlush vectorFloat

instructionVectorFloatEq :: State -> State
instructionVectorFloatEq = instructionEq vectorFloat

instructionVectorFloatStackDepth :: State -> State
instructionVectorFloatStackDepth = instructionStackDepth vectorFloat

instructionVectorFloatYank :: State -> State
instructionVectorFloatYank = instructionYank vectorFloat

instructionVectorFloatYankDup :: State -> State
instructionVectorFloatYankDup = instructionYankDup vectorFloat

instructionVectorFloatIsStackEmpty :: State -> State
instructionVectorFloatIsStackEmpty = instructionIsStackEmpty vectorFloat

instructionVectorFloatShove :: State -> State
instructionVectorFloatShove = instructionShove vectorFloat

instructionVectorFloatShoveDup :: State -> State
instructionVectorFloatShoveDup = instructionShoveDup vectorFloat

instructionVectorFloatSort :: State -> State
instructionVectorFloatSort = instructionVectorSort vectorFloat

instructionVectorFloatSortReverse :: State -> State
instructionVectorFloatSortReverse = instructionVectorSortReverse vectorFloat

instructionVectorFloatDupItems :: State -> State
instructionVectorFloatDupItems = instructionDupItems vectorFloat
