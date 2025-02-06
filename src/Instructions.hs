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

intInstructions :: [Gene]
intInstructions = [
    StateFunc (instructionIntFromFloat, "instructionIntFromFloat"),
    StateFunc (instructionIntFromBool, "instructionIntFromBool"),
    StateFunc (instructionIntAdd, "instructionIntAdd"),
    StateFunc (instructionIntSub, "instructionIntSub"),
    StateFunc (instructionIntMul, "instructionIntMul"),
    StateFunc (instructionIntDiv, "instructionIntDiv"),
    StateFunc (instructionIntMod, "instructionIntMod"),
    StateFunc (instructionIntMin, "instructionIntMin"),
    StateFunc (instructionIntMax, "instructionIntMax"),
    StateFunc (instructionIntInc, "instructionIntInc"),
    StateFunc (instructionIntDec, "instructionIntDec"),
    StateFunc (instructionIntLT, "instructionIntLT"),
    StateFunc (instructionIntGT, "instructionIntGT"),
    StateFunc (instructionIntLTE, "instructionIntLTE"),
    StateFunc (instructionIntGTE, "instructionIntGTE"),
    StateFunc (instructionIntDup, "instructionIntDup"),
    StateFunc (instructionIntPop, "instructionIntPop"),
    StateFunc (instructionIntDupN, "instructionIntDupN"),
    StateFunc (instructionIntSwap, "instructionIntSwap"),
    StateFunc (instructionIntRot, "instructionIntRot"),
    StateFunc (instructionIntFlush, "instructionIntFlush"),
    StateFunc (instructionIntEq, "instructionIntEq"),
    StateFunc (instructionIntYank, "instructionIntYank"),
    StateFunc (instructionIntYankDup, "instructionIntYankDup"),
    StateFunc (instructionIntShove, "instructionIntShove"),
    StateFunc (instructionIntIsEmpty, "instructionIntIsEmpty")
  ]
