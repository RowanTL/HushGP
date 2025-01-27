module Instructions.VectorIntInstructions where

import Instructions.GenericInstructions
import State

instructionIntVectorConcat :: State -> State
instructionIntVectorConcat state = instructionConcat state intVector
