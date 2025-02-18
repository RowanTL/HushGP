-- import HushGP.Instructions
-- import HushGP.Push
import HushGP.PushTests
-- import HushGP.State
import Test.QuickCheck

-- import Data.List
-- import Control.Lens

-- import Debug.Trace

pushTestArgs :: Args
pushTestArgs = stdArgs {maxSize = 10}

-- These two used for ghci testing
-- For example (in ghci): qcw prop_myTest
qcw :: (Testable a) => a -> IO ()
qcw = quickCheckWith pushTestArgs

vcw :: (Testable a) => a -> IO ()
vcw = verboseCheckWith pushTestArgs

main :: IO ()
main = do
  qcw prop_IntAdd
  qcw prop_IntSub
  qcw prop_IntMul
  qcw prop_IntDiv
  qcw prop_IntMod
  qcw prop_IntFromFloat
  qcw prop_IntFromBool
  qcw prop_IntMin
  qcw prop_IntMax
  qcw prop_IntInc
  qcw prop_IntDec
  qcw prop_IntLT
  qcw prop_IntGT
  qcw prop_IntLTE
  qcw prop_IntGTE
  qcw prop_IntDup
  qcw prop_IntPop
