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

doRangeState =
  State
    { exec = [Block [IntGene 1, IntGene 6, StateFunc instructionIntAdd], Block [IntGene 4, IntGene 1, StateFunc instructionExecDoRange]]
    , int = []
    , float = []
    , string = ["abc"]
    , bool = []
    , parameter = []
    , input = Map.empty
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
loadedState5 = instructionExecDup exampleState
testResult10 = exec loadedState5 !! 0 == IntGene 5 && exec loadedState5 !! 1 == IntGene 5

-- Tests execDoRange
testResult11 = [1, 4, 7] == int (interpretExec doRangeState)

allTests = and [testResult1, testResult2, testResult3, testResult4, testResult5, testResult6, testResult7, testResult8, testResult9]
