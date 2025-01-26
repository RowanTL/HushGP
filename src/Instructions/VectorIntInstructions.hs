module Instructions.VectorIntInstructions where

import Instructions.GenericInstructions
import State

instructionVectorIntConcat :: State -> State
instructionVectorIntConcat state = instructionConcat state intVector
