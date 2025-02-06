module HushGP.Instructions.VectorIntInstructions where

import HushGP.Instructions.GenericInstructions
import HushGP.State

instructionVectorIntConcat :: State -> State
instructionVectorIntConcat state = instructionConcat state vectorInt

instructionVectorIntConj :: State -> State
instructionVectorIntConj state = instructionConj state int vectorInt

instructionVectorIntTakeN :: State -> State
instructionVectorIntTakeN state = instructionTakeN state vectorInt

instructionVectorIntSubVector :: State -> State
instructionVectorIntSubVector state = instructionSubVector state vectorInt

instructionVectorIntFirst :: State -> State
instructionVectorIntFirst state = instructionVectorFirst state int vectorInt

instructionVectorIntLast :: State -> State
instructionVectorIntLast state = instructionVectorLast state int vectorInt

instructionVectorIntNth :: State -> State
instructionVectorIntNth state = instructionVectorNth state int vectorInt

instructionVectorIntRest :: State -> State
instructionVectorIntRest state = instructionRest state vectorInt

instructionVectorIntButLast :: State -> State
instructionVectorIntButLast state = instructionButLast state vectorInt

instructionVectorIntLength :: State -> State
instructionVectorIntLength state = instructionLength state vectorInt

instructionVectorIntReverse :: State -> State
instructionVectorIntReverse state = instructionReverse state vectorInt

instructionVectorIntPushAll :: State -> State
instructionVectorIntPushAll state = instructionPushAll state int vectorInt

instructionVectorIntMakeEmpty :: State -> State
instructionVectorIntMakeEmpty state = instructionVectorMakeEmpty state vectorInt

instructionVectorIntIsEmpty :: State -> State
instructionVectorIntIsEmpty state = instructionVectorIsEmpty state vectorInt

instructionVectorIntIndexOf :: State -> State
instructionVectorIntIndexOf state = instructionVectorIndexOf state int vectorInt

instructionVectorIntOccurrencesOf :: State -> State
instructionVectorIntOccurrencesOf state = instructionVectorOccurrencesOf state int vectorInt

instructionVectorIntSetNth :: State -> State
instructionVectorIntSetNth state = instructionVectorSetNth state int vectorInt

instructionVectorIntReplace :: State -> State
instructionVectorIntReplace state = instructionVectorReplace state int vectorInt

instructionVectorIntReplaceFirst :: State -> State
instructionVectorIntReplaceFirst state = instructionVectorReplaceFirst state int vectorInt

instructionVectorIntRemove :: State -> State
instructionVectorIntRemove state = instructionVectorRemove state int vectorInt

instructionVectorIntIterate :: State -> State
instructionVectorIntIterate state = instructionVectorIterate state int vectorInt GeneVectorInt instructionVectorIntIterate "instructionVectorIntIterate"

instructionVectorIntPop :: State -> State
instructionVectorIntPop state = instructionPop state vectorChar

instructionVectorIntDup :: State -> State
instructionVectorIntDup state = instructionDup state vectorChar

instructionVectorIntDupN :: State -> State
instructionVectorIntDupN state = instructionDupN state vectorChar

instructionVectorIntSwap :: State -> State
instructionVectorIntSwap state = instructionSwap state vectorChar

instructionVectorIntRot :: State -> State
instructionVectorIntRot state = instructionRot state vectorChar

instructionVectorIntFlush :: State -> State
instructionVectorIntFlush state = instructionFlush state vectorChar

instructionVectorIntEq :: State -> State
instructionVectorIntEq state = instructionEq state vectorChar

instructionVectorIntStackDepth :: State -> State
instructionVectorIntStackDepth state = instructionStackDepth state vectorChar

instructionVectorIntYank :: State -> State
instructionVectorIntYank state = instructionYank state vectorChar

instructionVectorIntYankDup :: State -> State
instructionVectorIntYankDup state = instructionYankDup state vectorChar

instructionVectorIntStackIsEmpty :: State -> State
instructionVectorIntStackIsEmpty state = instructionIsEmpty state vectorChar

instructionVectorIntShove :: State -> State
instructionVectorIntShove state = instructionShove state vectorChar

instructionVectorIntShoveDup :: State -> State
instructionVectorIntShoveDup state = instructionShoveDup state vectorChar
