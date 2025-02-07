module HushGP.Instructions.VectorCharInstructions where

import HushGP.State
import HushGP.Instructions.GenericInstructions

instructionVectorCharConcat :: State -> State
instructionVectorCharConcat state = instructionConcat state vectorChar

instructionVectorCharConj :: State -> State
instructionVectorCharConj state = instructionConj state char vectorChar

instructionVectorCharTakeN :: State -> State
instructionVectorCharTakeN state = instructionTakeN state vectorChar

instructionVectorCharSubVector :: State -> State
instructionVectorCharSubVector state = instructionSubVector state vectorChar

instructionVectorCharFirst :: State -> State
instructionVectorCharFirst state = instructionVectorFirst state char vectorChar

instructionVectorCharLast :: State -> State
instructionVectorCharLast state = instructionVectorLast state char vectorChar

instructionVectorCharNth :: State -> State
instructionVectorCharNth state = instructionVectorNth state char vectorChar

instructionVectorCharRest :: State -> State
instructionVectorCharRest state = instructionRest state vectorChar

instructionVectorCharButLast :: State -> State
instructionVectorCharButLast state = instructionButLast state vectorChar

instructionVectorCharLength :: State -> State
instructionVectorCharLength state = instructionLength state vectorChar

instructionVectorCharReverse :: State -> State
instructionVectorCharReverse state = instructionReverse state vectorChar

instructionVectorCharPushAll :: State -> State
instructionVectorCharPushAll state = instructionPushAll state char vectorChar

instructionVectorCharMakeEmpty :: State -> State
instructionVectorCharMakeEmpty state = instructionVectorMakeEmpty state vectorChar

instructionVectorCharIsEmpty :: State -> State
instructionVectorCharIsEmpty state = instructionVectorIsEmpty state vectorChar

instructionVectorCharIndexOf :: State -> State
instructionVectorCharIndexOf state = instructionVectorIndexOf state char vectorChar

instructionVectorCharOccurrencesOf :: State -> State
instructionVectorCharOccurrencesOf state = instructionVectorOccurrencesOf state char vectorChar

instructionVectorCharSetNth :: State -> State
instructionVectorCharSetNth state = instructionVectorSetNth state char vectorChar

instructionVectorCharReplace :: State -> State
instructionVectorCharReplace state = instructionVectorReplace state char vectorChar

instructionVectorCharReplaceFirst :: State -> State
instructionVectorCharReplaceFirst state = instructionVectorReplaceFirst state char vectorChar

instructionVectorCharRemove :: State -> State
instructionVectorCharRemove state = instructionVectorRemove state char vectorChar

instructionVectorCharIterate :: State -> State
instructionVectorCharIterate state = instructionVectorIterate state char vectorChar GeneVectorChar instructionVectorCharIterate "instructionVectorCharIterate"

instructionVectorCharPop :: State -> State
instructionVectorCharPop state = instructionPop state vectorChar

instructionVectorCharDup :: State -> State
instructionVectorCharDup state = instructionDup state vectorChar

instructionVectorCharDupN :: State -> State
instructionVectorCharDupN state = instructionDupN state vectorChar

instructionVectorCharSwap :: State -> State
instructionVectorCharSwap state = instructionSwap state vectorChar

instructionVectorCharRot :: State -> State
instructionVectorCharRot state = instructionRot state vectorChar

instructionVectorCharFlush :: State -> State
instructionVectorCharFlush state = instructionFlush state vectorChar

instructionVectorCharEq :: State -> State
instructionVectorCharEq state = instructionEq state vectorChar

instructionVectorCharStackDepth :: State -> State
instructionVectorCharStackDepth state = instructionStackDepth state vectorChar

instructionVectorCharYank :: State -> State
instructionVectorCharYank state = instructionYank state vectorChar

instructionVectorCharYankDup :: State -> State
instructionVectorCharYankDup state = instructionYankDup state vectorChar

instructionVectorCharStackIsEmpty :: State -> State
instructionVectorCharStackIsEmpty state = instructionIsEmpty state vectorChar

instructionVectorCharShove :: State -> State
instructionVectorCharShove state = instructionShove state vectorChar

instructionVectorCharShoveDup :: State -> State
instructionVectorCharShoveDup state = instructionShoveDup state vectorChar

instructionVectorCharSort :: State -> State
instructionVectorCharSort = instructionVectorSort vectorChar

instructionVectorCharSortReverse :: State -> State
instructionVectorCharSortReverse = instructionVectorSortReverse vectorChar

instructionVectorCharDupItems :: State -> State
instructionVectorCharDupItems = instructionDupItems vectorChar
