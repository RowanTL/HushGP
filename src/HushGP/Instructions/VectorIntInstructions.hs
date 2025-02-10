module HushGP.Instructions.VectorIntInstructions where

import HushGP.Instructions.GenericInstructions
import HushGP.State

instructionVectorIntConcat :: State -> State
instructionVectorIntConcat = instructionConcat vectorInt

instructionVectorIntConj :: State -> State
instructionVectorIntConj = instructionConj int vectorInt

instructionVectorIntTakeN :: State -> State
instructionVectorIntTakeN = instructionTakeN vectorInt

instructionVectorIntSubVector :: State -> State
instructionVectorIntSubVector = instructionSubVector vectorInt

instructionVectorIntFirst :: State -> State
instructionVectorIntFirst = instructionVectorFirst int vectorInt

instructionVectorIntLast :: State -> State
instructionVectorIntLast = instructionVectorLast int vectorInt

instructionVectorIntNth :: State -> State
instructionVectorIntNth = instructionVectorNth int vectorInt

instructionVectorIntRest :: State -> State
instructionVectorIntRest = instructionRest vectorInt

instructionVectorIntButLast :: State -> State
instructionVectorIntButLast = instructionButLast vectorInt

instructionVectorIntLength :: State -> State
instructionVectorIntLength = instructionLength vectorInt

instructionVectorIntReverse :: State -> State
instructionVectorIntReverse = instructionReverse vectorInt

instructionVectorIntPushAll :: State -> State
instructionVectorIntPushAll = instructionPushAll int vectorInt

instructionVectorIntMakeEmpty :: State -> State
instructionVectorIntMakeEmpty = instructionVectorMakeEmpty vectorInt

instructionVectorIntIsEmpty :: State -> State
instructionVectorIntIsEmpty = instructionVectorIsEmpty vectorInt

instructionVectorIntIndexOf :: State -> State
instructionVectorIntIndexOf = instructionVectorIndexOf int vectorInt

instructionVectorIntOccurrencesOf :: State -> State
instructionVectorIntOccurrencesOf = instructionVectorOccurrencesOf int vectorInt

instructionVectorIntSetNth :: State -> State
instructionVectorIntSetNth = instructionVectorSetNth int vectorInt

instructionVectorIntReplace :: State -> State
instructionVectorIntReplace = instructionVectorReplace int vectorInt Nothing

instructionVectorIntReplaceFirst :: State -> State
instructionVectorIntReplaceFirst = instructionVectorReplace int vectorInt (Just 1)

instructionVectorIntRemove :: State -> State
instructionVectorIntRemove = instructionVectorRemove int vectorInt

instructionVectorIntIterate :: State -> State
instructionVectorIntIterate = instructionVectorIterate int vectorInt GeneVectorInt instructionVectorIntIterate "instructionVectorIntIterate"

instructionVectorIntPop :: State -> State
instructionVectorIntPop = instructionPop vectorChar

instructionVectorIntDup :: State -> State
instructionVectorIntDup = instructionDup vectorChar

instructionVectorIntDupN :: State -> State
instructionVectorIntDupN = instructionDupN vectorChar

instructionVectorIntSwap :: State -> State
instructionVectorIntSwap = instructionSwap vectorChar

instructionVectorIntRot :: State -> State
instructionVectorIntRot = instructionRot vectorChar

instructionVectorIntFlush :: State -> State
instructionVectorIntFlush = instructionFlush vectorChar

instructionVectorIntEq :: State -> State
instructionVectorIntEq = instructionEq vectorChar

instructionVectorIntStackDepth :: State -> State
instructionVectorIntStackDepth = instructionStackDepth vectorChar

instructionVectorIntYank :: State -> State
instructionVectorIntYank = instructionYank vectorChar

instructionVectorIntYankDup :: State -> State
instructionVectorIntYankDup = instructionYankDup vectorChar

instructionVectorIntIsStackEmpty :: State -> State
instructionVectorIntIsStackEmpty = instructionIsStackEmpty vectorChar

instructionVectorIntShove :: State -> State
instructionVectorIntShove = instructionShove vectorChar

instructionVectorIntShoveDup :: State -> State
instructionVectorIntShoveDup = instructionShoveDup vectorChar

instructionVectorIntSort :: State -> State
instructionVectorIntSort = instructionVectorSort vectorInt

instructionVectorIntSortReverse :: State -> State
instructionVectorIntSortReverse = instructionVectorSortReverse vectorInt

instructionVectorIntDupItems :: State -> State
instructionVectorIntDupItems = instructionDupItems vectorInt
