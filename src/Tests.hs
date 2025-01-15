module Tests where

import Push

exampleState =
  State
    { exec = [IntGene 5, StateFunc instructionParameterLoad, StateFunc instructionIntAdd],
      int = [2, 6, 3],
      float = [1.2, 1.7],
      bool = [True, False],
      string = ["Hello", "Push"],
      parameter = [IntGene 1, StringGene "Hi", BoolGene True, FloatGene 1.3]
    }

testResult1 = [8, 3] == int (instructionIntAdd exampleState)

testResult2 = [4, 3] == int (instructionIntSubtract exampleState)

testResult3 = [12, 3] == int (instructionIntMultiply exampleState)

testResult4 = [3, 3] == int (instructionIntDivide exampleState)

testResult5 = [6, 2, 6, 3] == int (interpretExec exampleState)

loadedState = loadProgarm [IntGene 6, IntGene 6, StateFunc instructionIntAdd] emptyState

testResult6 = [12] == int (interpretExec loadedState)

allTests = and [testResult1, testResult2, testResult3, testResult4, testResult5, testResult6]
