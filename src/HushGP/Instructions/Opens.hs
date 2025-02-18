module HushGP.Instructions.Opens where

import HushGP.State
import Data.Map qualified as Map
import HushGP.Instructions.GenericInstructions
import HushGP.Instructions.ExecInstructions
import HushGP.Instructions.StringInstructions
import HushGP.Instructions.VectorIntInstructions
import HushGP.Instructions.VectorBoolInstructions
import HushGP.Instructions.VectorFloatInstructions
import HushGP.Instructions.VectorStringInstructions
import HushGP.Instructions.VectorCharInstructions

-- |A Map that takes a Gene and returns how many Blocks it opens.
-- To be used in plushy conversion.
instructionOpens :: Map.Map Gene Int
instructionOpens = Map.fromList [
    (StateFunc (instructionExecIf, "instructionExecIf"), 2),
    (StateFunc (instructionExecDup, "instructionExecDup"), 1),
    (StateFunc (instructionExecDupN, "instructionExecDupN"), 1),
    (StateFunc (instructionExecPop, "instructionExecPop"), 1),
    (StateFunc (instructionExecSwap, "instructionExecSwap"), 2),
    (StateFunc (instructionExecRot, "instructionExecRot"), 3),
    (StateFunc (instructionExecShove, "instructionExecShove"), 1),
    (StateFunc (instructionExecShoveDup, "instructionExecShoveDup"), 1),
    (StateFunc (instructionExecDoRange, "instructionExecDoRange"), 1),
    (StateFunc (instructionExecDoCount, "instructionExecDoCount"), 1),
    (StateFunc (instructionExecDoTimes, "instructionExecDoTimes"), 1),
    (StateFunc (instructionExecWhile, "instructionExecWhile"), 1),
    (StateFunc (instructionExecDoWhile, "instructionExecDoWhile"), 1),
    (StateFunc (instructionExecWhen, "instructionExecWhen"), 1),
    (StateFunc (instructionExecK, "instructionExecK"), 2),
    (StateFunc (instructionExecS, "instructionExecS"), 3),
    (StateFunc (instructionExecY, "instructionExecY"), 1),
    (StateFunc (instructionStringIterate, "instructionStringIterate"), 1),
    (StateFunc (instructionVectorIntIterate, "instructionVectorIntIterate"), 1),
    (StateFunc (instructionVectorFloatIterate, "instructionVectorFloatIterate"), 1),
    (StateFunc (instructionVectorStringIterate, "instructionVectorStringIterate"), 1),
    (StateFunc (instructionVectorBoolIterate, "instructionVectorBoolIterate"), 1),
    (StateFunc (instructionVectorCharIterate, "instructionVectorCharIterate"), 1),
    (StateFunc (instructionNoOpBlock, "instructionNoOpBlock"), 1)
  ]
