module HushGP.Instructions
  ( module HushGP.Instructions.GenericInstructions,
    module HushGP.Instructions.IntInstructions,
    module HushGP.Instructions.FloatInstructions,
    module HushGP.Instructions.StringInstructions,
    module HushGP.Instructions.CharInstructions,
    module HushGP.Instructions.CodeInstructions,
    module HushGP.Instructions.ExecInstructions,
    module HushGP.Instructions.BoolInstructions,
    module HushGP.Instructions.VectorIntInstructions,
    module HushGP.Instructions.VectorFloatInstructions,
    module HushGP.Instructions.VectorStringInstructions,
    module HushGP.Instructions.VectorBoolInstructions,
    module HushGP.Instructions.VectorCharInstructions,
    allInstructions,
  )
where

import HushGP.Instructions.BoolInstructions
import HushGP.Instructions.CharInstructions
import HushGP.Instructions.CodeInstructions
import HushGP.Instructions.ExecInstructions
import HushGP.Instructions.FloatInstructions
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.IntInstructions
import HushGP.Instructions.StringInstructions
import HushGP.Instructions.VectorBoolInstructions
import HushGP.Instructions.VectorCharInstructions
import HushGP.Instructions.VectorFloatInstructions
import HushGP.Instructions.VectorIntInstructions
import HushGP.Instructions.VectorStringInstructions
import HushGP.State

noOpStateFunc :: Gene
noOpStateFunc = StateFunc (instructionNoOp, "instructionNoOp")

noOpStateFuncBlock :: Gene
noOpStateFuncBlock = StateFunc (instructionNoOpBlock, "instructionNoOpBlock")

-- | All of the instructions declared in all the instruction submodules
allInstructions :: [Gene]
allInstructions =
  noOpStateFunc : noOpStateFuncBlock : allIntInstructions
    <> allFloatInstructions
    <> allBoolInstructions
    <> allCharInstructions
    <> allCodeInstructions
    <> allExecInstructions
    <> allStringInstructions
    <> allVectorIntInstructions
    <> allVectorFloatInstructions
    <> allVectorCharInstructions
    <> allVectorStringInstructions
    <> allVectorBoolInstructions
