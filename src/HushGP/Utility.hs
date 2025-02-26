module HushGP.Utility where

import Control.Monad
import HushGP.State
import System.Random

-- | Generates a single random instruction from a list of instructions.
randomInstruction :: [Gene] -> IO Gene
randomInstruction instructions = do
  impureGen <- initStdGen
  return $ instructions !! fst (uniformR (0, length instructions - 1) impureGen)

-- | Generates a list of random instructions from a list of instructions passed in.
randomInstructions :: Int -> [Gene] -> IO [Gene]
randomInstructions amt instructions = replicateM amt (randomInstruction instructions)

-- |Utility function: Used for indexed training data. Takes the first element of triple.
tfst :: (a, b, c) -> a
tfst (x, _, _) = x

-- |Utility function: Used for indexed training data. Takes the second element of triple.
tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

-- |Utility function: Used for indexed training data. Takes the third element of triple.
-- The third element in the context of indexed training data represents the index assigned.
thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- |Utility function: Converts a tuple to a triple with a passed value.
tupleToTriple :: (a, b) -> c -> (a, b, c)
tupleToTriple (x, y) z = (x, y, z)
