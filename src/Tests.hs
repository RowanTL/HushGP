module Tests where

import Push

exampleState =
  State
    { exec = [IntGene 5, StateFunc instructionParameterLoad, StateFunc instructionIntAdd],
      int = [1, 2, 3],
      float = [1.2, 1.7],
      bool = [True, False],
      string = ["Hello", "Push"],
      parameter = [IntGene 1, StringGene "Hi", BoolGene True, FloatGene 1.3]
    }

-- intAdd
testResult1 = [3, 3] == int (instructionIntAdd exampleState)

-- interpretExec
testResult2 = [6,1,2,3] == int (interpretExec exampleState)

-- This nukes the exec stack, just as an example of how to load at the start.
loadedState = loadProgarm [IntGene 6, IntGene 6, StateFunc instructionParameterLoad, StateFunc instructionIntAdd] emptyState

-- interpretExec
testResult3 = [12] == int (interpretExec loadedState)

