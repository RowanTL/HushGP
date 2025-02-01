import Instructions
import State
import Test.QuickCheck

-- import Control.Lens

myArgs =
  Args
    { replay = Nothing,
      maxSuccess = 100,
      maxDiscardRatio = 10,
      maxSize = 10,
      chatty = True,
      maxShrinks = 500
    }

-- quickCheckWith myArgs prop_IntAdd

-- These two used for ghci testing
qcw :: Testable a => a-> IO ()
qcw = quickCheckWith myArgs

vcw :: Testable a => a-> IO ()
vcw = verboseCheckWith myArgs

-- Running this with a large max size leads quickCheck to hang, and that's bad
prop_IntAdd :: State -> Bool
prop_IntAdd state@(State {_int = i1 : i2 : _}) = i1 + i2 == head (_int (instructionIntAdd state))
prop_IntAdd state = state == instructionIntAdd state

main :: IO ()
main = do
  putStrLn "hello"
