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
instructionVectorFloatIterate state = instructionVectorIterate state float vectorFloat GeneVectorFloat instructionVectorFloatIterate

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

instructionVectorFloatMean :: State -> State
instructionVectorFloatMean state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = mean vf : fs}
    []        -> state  -- Do nothing if _vectorFloat is empty
  where
    mean [] = 0
    mean xs = sum xs / fromIntegral (length xs)

instructionVectorFloatMax :: State -> State
instructionVectorFloatMax state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = maximum vf : fs}
    []        -> state

instructionVectorFloatMin :: State -> State
instructionVectorFloatMin state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = minimum vf : fs}
    []        -> state

instructionVectorFloatSum :: State -> State
instructionVectorFloatSum state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = sum vf : fs}
    []        -> state

instructionVectorFloatMode :: State -> State
instructionVectorFloatMode state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = mode vf : fs}
    []        -> state
  where
    mode [] = 0
    mode xs = head $ maximumBy (comparing length) (group (sort xs))

instructionVectorFloatNorm :: State -> State
instructionVectorFloatNorm state@(State {_vectorFloat = vfs, _float = fs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = vfs', _float = realToFrac (norm vf) : fs}
    []        -> state
  where
    norm xs = norm_2 (vector xs)

instructionVectorFloatCummulativeMean :: State -> State
instructionVectorFloatCummulativeMean state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = zipWith (/) (scanl1 (+) (map fromIntegral vf)) [1..] : vfs'}
    []        -> state

instructionVectorFloatCummulativeSum :: State -> State
instructionVectorFloatCummulativeSum state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = scanl1 (+) vf : vfs'}
    []        -> state

instructionVectorFloatCummulativeMax :: State -> State
instructionVectorFloatCummulativeMax state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = scanl1 maximum vf : vfs'}
    []        -> state

instructionVectorFloatCummulativeMin :: State -> State
instructionVectorFloatCummulativeMin state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = scanl1 minimum vf : vfs'}
    []        -> state

instructionVectorFloatExp :: State -> State
instructionVectorFloatExp state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map exp vf : vfs'}
    []        -> state


instructionVectorFloatLog :: State -> State
instructionVectorFloatLog state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map log vf : vfs'}
    []        -> state

instructionVectorFloatCos :: State -> State
instructionVectorFloatCos state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map cos vf : vfs'}
    []        -> state

instructionVectorFloatSin :: State -> State
instructionVectorFloatSin state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map sin vf : vfs'}
    []        -> state

instructionVectorFloatAbs :: State -> State
instructionVectorFloatAbs state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map abs vf : vfs'}
    []        -> state

instructionVectorFloatSquare :: State -> State
instructionVectorFloatSquare state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map (^2) vf : vfs'}
    []        -> state

instructionVectorFloatCube :: State -> State
instructionVectorFloatCube state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map (^3) vf : vfs'}
    []        -> state

instructionVectorFloatSqrt :: State -> State
instructionVectorFloatSqrt state@(State {_vectorFloat = vfs}) =
  case vfs of
    (vf:vfs') -> state {_vectorFloat = map (sqrt . fromIntegral) vf : vfs'}
    []        -> state