import BinaryConversions
import Numeric.Natural (Natural)
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural ()

test :: [Natural] -> Bool
test nums = nums == map (binToNat . natToBin) nums

main :: IO ()
main = quickCheck test
