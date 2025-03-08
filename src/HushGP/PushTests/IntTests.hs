module HushGP.PushTests.IntTests where

import Data.Char
import HushGP.State
import HushGP.PushTests.GenericTests
import HushGP.Instructions.IntInstructions
-- import Control.Lens hiding (uncons)
import System.Environment
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "10"
  setEnv "TASTY_QUICKCHECK_VERBOSE" "False"
  defaultMain intTests

-- |Holds the tree for property and unit tests.
intTests :: TestTree
intTests = testGroup "All Int Tests" [propIntTests]

-- |Property int tests.
propIntTests :: TestTree
propIntTests = testGroup "Property Int Tests"
  [ 
    QC.testProperty "Property Int Add test" prop_IntAdd
  , QC.testProperty "Property Int Sub test" prop_IntSub
  , QC.testProperty "Property Int SubOpp test" prop_IntSubOpp
  , QC.testProperty "Property Int Multiply test" prop_IntMul
  , QC.testProperty "Property Int Div test" prop_IntDiv
  , QC.testProperty "Property Int Div Opp test" prop_IntDivOpp
  , QC.testProperty "Property Int Mod test" prop_IntMod
  , QC.testProperty "Property Int Mod Opp test" prop_IntModOpp
  , QC.testProperty "Property IntFromFloat test" prop_IntFromFloat
  , QC.testProperty "Property IntFromBool test" prop_IntFromBool
  , QC.testProperty "Property IntFromChar test" prop_IntFromChar
  , QC.testProperty "Property IntFromString test" prop_IntFromString
  , QC.testProperty "Property IntMin test" prop_IntMin
  , QC.testProperty "Property IntMax test" prop_IntMax
  , QC.testProperty "Property IntInc test" prop_IntInc
  , QC.testProperty "Property IntDec test" prop_IntDec
  , QC.testProperty "Property IntLT test" prop_IntLT
  , QC.testProperty "Property IntGT test" prop_IntGT
  , QC.testProperty "Property IntLTE test" prop_IntLTE
  , QC.testProperty "Property IntGTE test" prop_IntGTE
  , QC.testProperty "Property IntDup test" prop_IntDup
  , QC.testProperty "Property IntPop test" prop_IntPop
  , QC.testProperty "Property IntDupN test" prop_IntDupN
  , QC.testProperty "Property IntSwap test" prop_IntSwap
  , QC.testProperty "Property IntRot test" prop_IntRot
  , QC.testProperty "Property IntFlush test" prop_IntFlush
  , QC.testProperty "Property IntEQ test" prop_IntEq
  , QC.testProperty "Property IntStackDepth test" prop_IntStackDepth
  , QC.testProperty "Property IntYank test" prop_IntYank
  , QC.testProperty "Property IntYankDup test" prop_IntYankDup
  , QC.testProperty "Property IntShove test" prop_IntShove
  , QC.testProperty "Property IntShoveDup test" prop_IntShoveDup
  , QC.testProperty "Property IntIsStackEmpty test" prop_IntIsStackEmpty
  , QC.testProperty "Property IntDupItems test" prop_IntDupItems
  ]

prop_IntAdd :: State -> Property
prop_IntAdd = diff1Test int instructionIntAdd 1 2

prop_IntSub :: State -> Property
prop_IntSub = diff1Test int instructionIntSub 1 2

prop_IntSubOpp :: State -> Property
prop_IntSubOpp = diff1Test int instructionIntSubOpp 1 2

prop_IntMul :: State -> Property
prop_IntMul = diff1Test int instructionIntMul 1 2

prop_IntDiv :: State -> Property
prop_IntDiv state@(State {_int = 0 : _}) = state === instructionIntDiv state
prop_IntDiv state = diff1Test int instructionIntDiv 1 2 state

prop_IntDivOpp :: State -> Property
prop_IntDivOpp state@(State {_int = _ : 0 : _}) = state === instructionIntDivOpp state
prop_IntDivOpp state = diff1Test int instructionIntDivOpp 1 2 state

prop_IntMod :: State -> Property
prop_IntMod state@(State {_int = 0 : _}) = state === instructionIntMod state
prop_IntMod state = diff1Test int instructionIntMod 1 2 state

prop_IntModOpp :: State -> Property
prop_IntModOpp state@(State {_int = _ : 0 : _}) = state === instructionIntModOpp state
prop_IntModOpp state = diff1Test int instructionIntModOpp 1 2 state

prop_IntFromFloat :: State -> Property
prop_IntFromFloat = diff2Test float int instructionIntFromFloat 1 1 1

prop_IntFromBool :: State -> Property
prop_IntFromBool = diff2Test bool int instructionIntFromBool 1 1 1

prop_IntFromChar :: State -> Property
prop_IntFromChar = diff2Test char int instructionIntFromChar 1 1 1

prop_IntFromString :: State -> Property
prop_IntFromString state@(State {_string = s1 : _}) = if all isDigit s1 then diff2Test string int instructionIntFromString 1 1 1 state else state === instructionIntFromString state
prop_IntFromString state = state === instructionIntFromString state

prop_IntMin :: State -> Property
prop_IntMin = diff1Test int instructionIntMin 1 2 

prop_IntMax :: State -> Property
prop_IntMax = diff1Test int instructionIntMax 1 2

prop_IntInc :: State -> Property
prop_IntInc = diff1Test int instructionIntInc 0 1

prop_IntDec :: State -> Property
prop_IntDec = diff1Test int instructionIntDec 0 1

prop_IntLT :: State -> Property
prop_IntLT = diff2Test int bool instructionIntLT 2 1 2

prop_IntGT :: State -> Property
prop_IntGT = diff2Test int bool instructionIntGT 2 1 2

prop_IntLTE :: State -> Property
prop_IntLTE = diff2Test int bool instructionIntLTE 2 1 2

prop_IntGTE :: State -> Property
prop_IntGTE = diff2Test int bool instructionIntGTE 2 1 2

prop_IntDup :: State -> Property
prop_IntDup = diff1Test int instructionIntDup (-1) 1

prop_IntPop :: State -> Property
prop_IntPop = diff1Test int instructionIntPop 1 1

prop_IntDupN :: State -> Property
prop_IntDupN state@(State {_int = i1 : _ : _}) = diff1Test int instructionIntDupN ((- fromIntegral (max 0 i1)) + 2) 2 state
prop_IntDupN state = state === instructionIntDupN state

prop_IntSwap :: State -> Property
prop_IntSwap = diff1Test int instructionIntSwap 0 1

prop_IntRot :: State -> Property
prop_IntRot = diff1Test int instructionIntRot 0 3

prop_IntFlush :: State -> Property
prop_IntFlush state@(State {_int = is})= diff1Test int instructionIntFlush (length is) 0 state

prop_IntEq :: State -> Property
prop_IntEq = diff2Test int bool instructionIntEq 2 1 2

prop_IntStackDepth :: State -> Property
prop_IntStackDepth = diff1Test int instructionIntStackDepth (-1) 0

prop_IntYank :: State -> Property
prop_IntYank = diff1Test int instructionIntYank 1 2 

prop_IntYankDup :: State -> Property
prop_IntYankDup = diff1Test int instructionIntYankDup 0 2

prop_IntShove :: State -> Property
prop_IntShove = diff1Test int instructionIntShove 1 1

prop_IntShoveDup :: State -> Property
prop_IntShoveDup = diff1Test int instructionIntShoveDup 0 1

prop_IntIsStackEmpty :: State -> Property
prop_IntIsStackEmpty = diff2Test int bool instructionIntIsStackEmpty 0 1 0

prop_IntDupItems :: State -> Property
prop_IntDupItems state@(State {_int = i1 : _ : _})= diff1Test int instructionIntDupItems ((- fromIntegral (max 0 i1)) + 2) 2 state
prop_IntDupItems state = state === instructionIntDupItems state
