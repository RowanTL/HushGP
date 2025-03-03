module HushGP.Utility where

import Data.List
import Control.Monad
import System.Random
import HushGP.State

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

headCases :: [Int] -> Int
headCases xs = case uncons xs of Just (y, _) -> y; _ -> error "Error: cases is empty!"
