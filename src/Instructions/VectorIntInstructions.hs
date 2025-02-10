module Instructions.VectorIntInstructions where

import Instructions.GenericInstructions
import State
import Data.List (maximumBy, group, sort)
import Data.Ord (comparing)
import Numeric.LinearAlgebra (vector, norm_2)

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
instructionVectorIntIterate state = instructionVectorIterate state int vectorInt GeneVectorInt instructionVectorIntIterate

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

instructionVectorIntMean :: State -> State
instructionVectorIntMean state@(State {_vectorInt = vis, _float = fs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _float = mean vi : fs}
    []        -> state  -- Do nothing if _vectorInt is empty
  where
    mean [] = 0
    mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

instructionVectorIntMax :: State -> State
instructionVectorIntMax state@(State {_vectorInt = vis, _int = is}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _int = maximum vi : is}
    []        -> state

instructionVectorIntMin :: State -> State
instructionVectorIntMin state@(State {_vectorInt = vis, _int = is}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _int = minimum vi : is}
    []        -> state

instructionVectorIntSum :: State -> State
instructionVectorIntSum state@(State {_vectorInt = vis, _int = is}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _int = sum vi : is}
    []        -> state

instructionVectorIntMode :: State -> State
instructionVectorIntMode state@(State {_vectorInt = vis, _int = is}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _int = mode vi : is}
    []        -> state
  where
    mode [] = 0
    mode xs = head $ maximumBy (comparing length) (group (sort xs))

instructionVectorIntNorm :: State -> State
instructionVectorIntNorm state@(State {_vectorInt = vis, _float = fs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _float = realToFrac (norm (map fromIntegral vi)) : fs}
    []        -> state
  where
    norm xs = norm_2 (vector xs)

instructionVectorIntCummulativeMean :: State -> State
instructionVectorIntCummulativeMean state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis, _vectorFloat = zipWith (/) (scanl1 (+) (map fromIntegral vi)) [1..] : vfs}
    []        -> state

instructionVectorIntCummulativeSum :: State -> State
instructionVectorIntCummulativeSum state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = scanl1 (+) vi : vis'}
    []        -> state

instructionVectorIntCummulativeMax :: State -> State
instructionVectorIntCummulativeMax state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = scanl1 maximum vi : vis'}
    []        -> state

instructionVectorIntCummulativeMin :: State -> State
instructionVectorIntCummulativeMin state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = scanl1 minimum vi : vis'}
    []        -> state

instructionVectorIntExp :: State -> State
instructionVectorIntExp state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _vectorFloat = map (exp . fromIntegral) vi : vfs}
    []        -> state


instructionVectorIntLog :: State -> State
instructionVectorIntLog state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _vectorFloat = map (log . fromIntegral) vi : vfs}
    []        -> state

instructionVectorIntCos :: State -> State
instructionVectorIntCos state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _vectorFloat = map (cos . fromIntegral) vi : vfs}
    []        -> state

instructionVectorIntSin :: State -> State
instructionVectorIntSin state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _vectorFloat = map (sin . fromIntegral) vi : vfs}
    []        -> state

instructionVectorIntAbs :: State -> State
instructionVectorIntAbs state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = map abs vi : vis'}
    []        -> state

instructionVectorIntSquare :: State -> State
instructionVectorIntSquare state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = map (^2) vi : vis'}
    []        -> state

instructionVectorIntCube :: State -> State
instructionVectorIntCube state@(State {_vectorInt = vis}) =
  case vis of
    (vi:vis') -> state {_vectorInt = map (^3) vi : vis'}
    []        -> state

instructionVectorIntSqrt :: State -> State
instructionVectorIntSqrt state@(State {_vectorInt = vis, _vectorFloat = vfs}) =
  case vis of
    (vi:vis') -> state {_vectorInt = vis', _vectorFloat = map (sqrt . fromIntegral) vi : vfs}
    []        -> state