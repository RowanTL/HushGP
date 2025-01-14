import BinaryConversions
import Control.Exception (assert)

main :: IO ()
main = do
  let nums = [1, 2 .. 10]
  assert (nums == map (binToNat . natToBin) nums) pure ()
