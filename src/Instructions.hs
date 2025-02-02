module Instructions
  ( module Instructions.GenericInstructions,
    module Instructions.IntInstructions,
    module Instructions.FloatInstructions,
    module Instructions.StringInstructions,
    module Instructions.CharInstructions,
    module Instructions.CodeInstructions,
    module Instructions.ExecInstructions,
    module Instructions.LogicalInstructions,
    module Instructions.VectorIntInstructions,
    module Instructions.VectorFloatInstructions,
    module Instructions.VectorStringInstructions,
    module Instructions.VectorLogicalInstructions,
    module Instructions.VectorCharInstructions,
    largeState,
  )
where

import Instructions.CharInstructions
import Instructions.CodeInstructions
import Instructions.ExecInstructions
import Instructions.FloatInstructions
import Instructions.GenericInstructions
import Instructions.IntInstructions
import Instructions.LogicalInstructions
import Instructions.StringInstructions
import Instructions.VectorCharInstructions
import Instructions.VectorFloatInstructions
import Instructions.VectorIntInstructions
import Instructions.VectorLogicalInstructions
import Instructions.VectorStringInstructions
import State
import Data.Map qualified as Map

largeState :: State
largeState =
  State
    { _exec = replicate 100000 $ StateFunc (instructionIntAdd, "instructionIntAdd"),
      _code = replicate 100 $ StateFunc (instructionIntAdd, "instructionIntAdd"),
      _int = [1..100200],
      _float = [1.0..120.00],
      _bool = replicate 100 True,
      _string = replicate 100 "",
      _char = replicate 100 'z',
      _parameter = [],
      _vectorInt = replicate 100 [1,2,3],
      _vectorFloat = replicate 100 [1.0, 2.0, 3.0],
      _vectorBool = replicate 100 [True, False],
      _vectorString = replicate 100 ["hello", "there"],
      _vectorChar = replicate 100 ['a','b','c'],
      _input = Map.empty
    }


