module Tests where

import qualified Data.Map as Map
import Push

exampleState =
  State
    { exec = [IntGene 5, StateFunc instructionParameterLoad, StateFunc instructionIntAdd],
      int = [2, 6, 3],
      float = [1.2, 1.7],
      bool = [True, False],
      string = ["Hello", "Push"],
      parameter = [IntGene 1, StringGene "Hi", BoolGene True, FloatGene 1.3],
      input = Map.fromList [("in0" , IntGene 1)]
    }

testResult1 = [8, 3] == int (instructionIntAdd exampleState)

testResult2 = [4, 3] == int (instructionIntSub exampleState)

testResult3 = [12, 3] == int (instructionIntMul exampleState)

testResult4 = [3, 3] == int (instructionIntDiv exampleState)

testResult5 = [6, 2, 6, 3] == int (interpretExec exampleState)

loadedState = loadProgram [IntGene 6, IntGene 6, StateFunc instructionIntAdd] emptyState
testResult6 = [12] == int (interpretExec loadedState)

loadedState2 = loadProgram [BoolGene True, StateFunc instructionExecIf, Block [IntGene 5, IntGene 6], Block [IntGene 7, IntGene 8]] emptyState
testResult7 = [6, 5] == int (interpretExec loadedState2)

loadedState3 = loadProgram [BoolGene False, StateFunc instructionExecIf, Block [IntGene 5, IntGene 6], Block [IntGene 7, IntGene 8]] emptyState
testResult8 = [8, 7] == int (interpretExec loadedState3)

-- Tests input map
loadedState4 = loadProgram [BoolGene False, PlaceInput "in0", StateFunc instructionIntAdd] exampleState
testResult9 = [3, 6, 3] == int (interpretExec loadedState4)

-- Tests execDup
loadedState5 = interpretExec $ loadProgram [StateFunc instructionExecDup, IntGene 2] emptyState
testResult10 = int loadedState5 !! 0 == 2 && int loadedState5 !! 1 == 2

-- Tests execDoRange
loadedState6 = loadProgram [IntGene 2, Block [IntGene 4, IntGene 1, StateFunc instructionExecDoRange], StateFunc instructionIntAdd] emptyState
testResult11 = [12] == int (interpretExec loadedState6)

allTests = and [testResult1, testResult2, testResult3, testResult4, testResult5, testResult6, testResult7, testResult8, testResult9]
