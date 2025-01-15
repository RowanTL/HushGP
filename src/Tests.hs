module Tests where

import Push

exampleState =
  State
    { exec = [IntGene 5, FloatGene 3.4, BoolGene True, StringGene "hi"],
      int = [1, 2, 3],
      float = [1.2, 1.7],
      bool = [True, False],
      string = ["Hello", "Push"],
      parameter = [IntGene 1, StringGene "Hi", BoolGene True, FloatGene 1.3]
    }

-- intAdd
testResult1 = [3, 3] == int (intAdd exampleState)

loaded = loadProgarm [IntGene 6, IntGene 6, StateFunc intAdd] emptyState

-- interpretExec
testResult2 = [12] == int (interpretExec loaded)
