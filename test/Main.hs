import Instructions
import Push
import State
import Test.QuickCheck
import PushTests
-- import Data.List
-- import Control.Lens 

-- import Debug.Trace

pushTestArgs :: Args
pushTestArgs = stdArgs{maxSize = 10}
  
-- These two used for ghci testing
-- For example (in ghci): qcw prop_myTest
qcw :: Testable a => a -> IO ()
qcw = quickCheckWith pushTestArgs

vcw :: Testable a => a -> IO ()
vcw = verboseCheckWith pushTestArgs
