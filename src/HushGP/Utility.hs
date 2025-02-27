module HushGP.Utility where

import Control.Monad
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
