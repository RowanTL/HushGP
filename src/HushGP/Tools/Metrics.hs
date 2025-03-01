module HushGP.Tools.Metrics where

import Data.List
import System.Random
import System.Random.Shuffle

-- |Maps minimum over the transposed [[Double]].
minOfColumns :: (Num a, Ord a) => [[a]] -> [a]
minOfColumns columns = map minimum (transpose columns)

-- |Returns the index of the maximum value in a list, randomly tiebreaking.
argMax :: Ord a => [a] -> IO Int
argMax xs = argMaxHead . shuffle' (elemIndices (maximum xs) xs) (length xs) <$> initStdGen

-- |Takes the first element from a list and returns an error as specified. For use with
-- the argMax function.
argMaxHead :: [a] -> a
argMaxHead xs = case uncons xs of
  Just (x, _) -> x
  _ -> error "Error: Head is empty in argMax!"
