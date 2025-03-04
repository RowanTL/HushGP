module HushGP.Utility where

import Control.Monad
import Data.List
import HushGP.State
import System.Random

-- | Generates a single random instruction from a list of instructions.
randomInstruction :: [Gene] -> IO Gene
randomInstruction instructions = do
  impureGen <- initStdGen
  pure $ instructions !! fst (uniformR (0, length instructions - 1) impureGen)

-- | Generates a list of random instructions from a list of instructions passed in.
randomInstructions :: Int -> [Gene] -> IO [Gene]
randomInstructions amt instructions = replicateM amt (randomInstruction instructions)

-- | Maps a function like the normal mapping function and also applies an index to it.
mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed = mapIndexed' 0

-- | Internals for mapIndexed, can supply a starting index for rather than just 0
-- with mapIndexed.
mapIndexed' :: Int -> (Int -> a -> b) -> [a] -> [b]
mapIndexed' _ _ [] = []
mapIndexed' count f (x : xs) = f count x : mapIndexed' (count + 1) f xs

-- | Returns a random element from a passed list. No generator required.
randElem :: [a] -> IO a
randElem xs = (xs !!) . fst . uniformR (0, length xs - 1) <$> initStdGen

-- | Used in some of the selection operations. Returns an error saying cases is empty.
headCases :: [Int] -> Int
headCases xs = case uncons xs of Just (y, _) -> y; _ -> error "Error: cases is empty!"

-- | Almost a constant but has some randomness inside. Double for more decimal precision.
--  Noise of mean of 0 and std dev of 1. This is a neat function to visualize on desmos!
gaussianNoiseFactor :: IO Double
gaussianNoiseFactor = do
  randDecimal0 <- fst . uniformR (0.0 :: Double, 1.0 :: Double) <$> initStdGen
  randDecimal1 <- fst . uniformR (0.0 :: Double, 1.0 :: Double) <$> initStdGen
  pure (sqrt ((-2.0) * log randDecimal0) * cos (2.0 * pi * randDecimal1))

-- | A random Int between 1 and 100 inclusive.
randOneToOneHundred :: IO Int
randOneToOneHundred = fst . uniformR (1 :: Int, 100 :: Int) <$> initStdGen

-- | A random Double between 0.1 and 1.0 inclusive.
randZeroToOne :: IO Double
randZeroToOne = fst . uniformR (0.1 :: Double, 1.0 :: Double) <$> initStdGen
