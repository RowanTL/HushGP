module HushGP.Instructions.VectorCharInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorCharConcat :: State -> State
instructionVectorCharConcat = instructionConcat vectorChar

instructionVectorCharConj :: State -> State
instructionVectorCharConj = instructionConj char vectorChar

instructionVectorCharTakeN :: State -> State
instructionVectorCharTakeN = instructionTakeN vectorChar

instructionVectorCharSubVector :: State -> State
instructionVectorCharSubVector = instructionSubVector vectorChar

instructionVectorCharFirst :: State -> State
instructionVectorCharFirst = instructionVectorFirst char vectorChar

instructionVectorCharLast :: State -> State
instructionVectorCharLast = instructionVectorLast char vectorChar

instructionVectorCharNth :: State -> State
instructionVectorCharNth = instructionVectorNth char vectorChar

instructionVectorCharRest :: State -> State
instructionVectorCharRest = instructionRest vectorChar

instructionVectorCharButLast :: State -> State
instructionVectorCharButLast = instructionButLast vectorChar

instructionVectorCharLength :: State -> State
instructionVectorCharLength = instructionLength vectorChar

instructionVectorCharReverse :: State -> State
instructionVectorCharReverse = instructionReverse vectorChar

instructionVectorCharPushAll :: State -> State
instructionVectorCharPushAll = instructionPushAll char vectorChar

instructionVectorCharMakeEmpty :: State -> State
instructionVectorCharMakeEmpty = instructionVectorMakeEmpty vectorChar

instructionVectorCharIsEmpty :: State -> State
instructionVectorCharIsEmpty = instructionVectorIsEmpty vectorChar

instructionVectorCharIndexOf :: State -> State
instructionVectorCharIndexOf = instructionVectorIndexOf char vectorChar

instructionVectorCharOccurrencesOf :: State -> State
instructionVectorCharOccurrencesOf = instructionVectorOccurrencesOf char vectorChar

instructionVectorCharSetNth :: State -> State
instructionVectorCharSetNth = instructionVectorSetNth char vectorChar

instructionVectorCharReplace :: State -> State
instructionVectorCharReplace = instructionVectorReplace char vectorChar Nothing

instructionVectorCharReplaceFirst :: State -> State
instructionVectorCharReplaceFirst = instructionVectorReplace char vectorChar (Just 1)

instructionVectorCharRemove :: State -> State
instructionVectorCharRemove = instructionVectorRemove char vectorChar

instructionVectorCharIterate :: State -> State
instructionVectorCharIterate = instructionVectorIterate char vectorChar GeneVectorChar instructionVectorCharIterate "instructionVectorCharIterate"

instructionVectorCharPop :: State -> State
instructionVectorCharPop = instructionPop vectorChar

instructionVectorCharDup :: State -> State
instructionVectorCharDup = instructionDup vectorChar

instructionVectorCharDupN :: State -> State
instructionVectorCharDupN = instructionDupN vectorChar

instructionVectorCharSwap :: State -> State
instructionVectorCharSwap = instructionSwap vectorChar

instructionVectorCharRot :: State -> State
instructionVectorCharRot = instructionRot vectorChar

instructionVectorCharFlush :: State -> State
instructionVectorCharFlush = instructionFlush vectorChar

instructionVectorCharEq :: State -> State
instructionVectorCharEq = instructionEq vectorChar

instructionVectorCharStackDepth :: State -> State
instructionVectorCharStackDepth = instructionStackDepth vectorChar

instructionVectorCharYank :: State -> State
instructionVectorCharYank = instructionYank vectorChar

instructionVectorCharYankDup :: State -> State
instructionVectorCharYankDup = instructionYankDup vectorChar

instructionVectorCharIsStackEmpty :: State -> State
instructionVectorCharIsStackEmpty = instructionIsStackEmpty vectorChar

instructionVectorCharShove :: State -> State
instructionVectorCharShove = instructionShove vectorChar

instructionVectorCharShoveDup :: State -> State
instructionVectorCharShoveDup = instructionShoveDup vectorChar

instructionVectorCharSort :: State -> State
instructionVectorCharSort = instructionVectorSort vectorChar

instructionVectorCharSortReverse :: State -> State
instructionVectorCharSortReverse = instructionVectorSortReverse vectorChar

instructionVectorCharDupItems :: State -> State
instructionVectorCharDupItems = instructionDupItems vectorChar
