module PushTests.IntTests where

import State
import Instructions.IntInstructions
import PushTests.GenericTests
import Data.List
import Control.Lens hiding (uncons)

prop_IntAdd :: State -> Bool
prop_IntAdd = arithmeticTest int instructionIntAdd (+)

prop_IntSub :: State -> Bool
prop_IntSub = arithmeticTest int instructionIntSub (-)

prop_IntMul :: State -> Bool
prop_IntMul = arithmeticTest int instructionIntMul (*)

prop_IntDiv :: State -> Bool
prop_IntDiv state@(State {_int = 0 : _}) = state == instructionIntDiv state
prop_IntDiv state = arithmeticTest int instructionIntDiv div state

prop_IntMod :: State -> Bool
prop_IntMod state@(State {_int = 0 : _}) = state == instructionIntMod state
prop_IntMod state = arithmeticTest int instructionIntMod mod state

prop_IntFromFloat :: State -> Bool
prop_IntFromFloat = typeFromType float int instructionIntFromFloat floor

prop_IntFromBool :: State -> Bool
prop_IntFromBool = typeFromType bool int instructionIntFromBool (\x -> if x then 1 else 0)

prop_IntMin :: State -> Bool
prop_IntMin = arithmeticTest int instructionIntMin min

prop_IntMax :: State -> Bool
prop_IntMax = arithmeticTest int instructionIntMax max

prop_IntInc :: State -> Bool
prop_IntInc = unaryTest int instructionIntInc (+1)

prop_IntDec :: State -> Bool
prop_IntDec = unaryTest int instructionIntDec (\x -> x - 1)
