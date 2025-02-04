module PushTests.IntTests where

import State
import Instructions.IntInstructions
import PushTests.GenericTests
import Control.Lens hiding (uncons)
import Test.QuickCheck

prop_IntAdd :: State -> Property
prop_IntAdd = aaa1Test int instructionIntAdd (+)

prop_IntSub :: State -> Property
prop_IntSub = aaa1Test int instructionIntSub (-)

prop_IntMul :: State -> Property
prop_IntMul = aaa1Test int instructionIntMul (*)

prop_IntDiv :: State -> Property
prop_IntDiv state@(State {_int = 0 : _}) = state === instructionIntDiv state
prop_IntDiv state = aaa1Test int instructionIntDiv div state

prop_IntMod :: State -> Property
prop_IntMod state@(State {_int = 0 : _}) = state === instructionIntMod state
prop_IntMod state = aaa1Test int instructionIntMod mod state

prop_IntFromFloat :: State -> Property
prop_IntFromFloat = ab1Test float int instructionIntFromFloat floor

prop_IntFromProperty :: State -> Property
prop_IntFromProperty = ab1Test bool int instructionIntFromBool (\x -> if x then 1 else 0)

prop_IntMin :: State -> Property
prop_IntMin = aaa1Test int instructionIntMin min

prop_IntMax :: State -> Property
prop_IntMax = aaa1Test int instructionIntMax max

prop_IntInc :: State -> Property
prop_IntInc = aa1Test int instructionIntInc (+1)

prop_IntDec :: State -> Property
prop_IntDec = aa1Test int instructionIntDec (\x -> x - 1)

prop_IntLT :: State -> Property
prop_IntLT = aab2Test int bool instructionIntLT (<)

prop_IntGT :: State -> Property
prop_IntGT = aab2Test int bool instructionIntGT (>)

prop_IntLTE :: State -> Property
prop_IntLTE = aab2Test int bool instructionIntLTE (<=)

prop_IntGTE :: State -> Property
prop_IntGTE = aab2Test int bool instructionIntGTE (>=)

prop_IntDup :: State -> Property
prop_IntDup = dupTest int instructionIntDup

prop_IntPop :: State -> Property
prop_IntPop = popTest int instructionIntPop

prop_IntDupN :: State -> Property
prop_IntDupN = dupTestN int instructionIntDupN

prop_IntSwap :: State -> Property
prop_IntSwap = swapTest int instructionIntSwap

prop_IntRot :: State -> Property
prop_IntRot = rotTest int instructionIntRot
