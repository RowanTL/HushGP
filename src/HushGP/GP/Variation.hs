module HushGP.GP.Variation where

import Control.Monad
import HushGP.State
import HushGP.GP.PushArgs
import HushGP.GP.Individual
import HushGP.Utility

-- |Performs a uniform crossover on two parents and returns the child.
-- Padding is placed to left of the shorter genome.
crossover :: [Gene] -> [Gene] -> IO [Gene]
crossover plushyA plushyB = do
  filter (CrossoverPadding /=) <$> zipWithM (\short long -> randOneToOneHundred >>= (\num -> if num < 50 then pure short else pure long)) shorterPadded longer
  where
    shorter :: [Gene]
    shorter = if length plushyA <= length plushyB then plushyA else plushyB
    longer :: [Gene]
    longer = if length plushyA > length plushyB then plushyA else plushyB
    lengthDiff :: Int
    lengthDiff = length longer - length shorter
    shorterPadded :: [Gene]
    shorterPadded = shorter <> replicate lengthDiff CrossoverPadding

-- |Alternates between placing genes from one parent to the other in a new child based on some random numbers.
alternation :: PushArgs -> [Gene] -> [Gene] -> IO [Gene]
alternation pushArgs plushyA plushyB = do
  randUsePlushyA <- randElem [True, False]
  alternation' pushArgs 0 randUsePlushyA [] (length plushyA + length plushyB) plushyA plushyB

-- |This is a chunker. The PushArgs used in the whole evolutionary run.
-- The first Int is used in the gaussian noise calculation and as a stop condition.
-- The Bool is used to determine which plushy is used to copy to the child.
-- The first [Gene] is the child being created recursively.
-- The second int is the iteration budget. Used to stop very long looping.
-- The second [Gene] is the first plushy parent.
-- The third [Gene] is the second plushy parent.
-- This returns the first [Gene] when the loop is complete.
alternation' :: PushArgs -> Int -> Bool -> [Gene] -> Int -> [Gene] -> [Gene] -> IO [Gene]
alternation' pushArgs@PushArgs{alternationRate = altRate, alignmentDeviation = alignDeviation} n usePlushyA !resultPlushy iterationBudget plushyA plushyB = do
  randNum <- randOneToOneHundred
  let nextAction
        | n >= length (if usePlushyA then plushyA else plushyB) || iterationBudget <= 0 = pure resultPlushy
        | randNum < altRate = do
            gNoiseFactor <- gaussianNoiseFactor
            alternation' pushArgs (max 0 (n + round (gNoiseFactor * alignDeviation))) (not usePlushyA) resultPlushy (pred iterationBudget) plushyA plushyB
        | otherwise = alternation' pushArgs (succ n) usePlushyA (resultPlushy <> [(if usePlushyA then plushyA else plushyB) !! n]) (pred iterationBudget) plushyA plushyB
  nextAction

-- |Performs a uniform crossover on two parents and returns the child.
-- Padding is placed to left of the shorter genome.
tailAlignedCrossover :: [Gene] -> [Gene] -> IO [Gene]
tailAlignedCrossover plushyA plushyB = do
  filter (CrossoverPadding /=) <$> zipWithM (\short long -> randOneToOneHundred >>= (\num -> if num < 50 then pure short else pure long)) shorterPadded longer
  where
    shorter :: [Gene]
    shorter = if length plushyA <= length plushyB then plushyA else plushyB
    longer :: [Gene]
    longer = if length plushyA > length plushyB then plushyA else plushyB
    lengthDiff :: Int
    lengthDiff = length longer - length shorter
    shorterPadded :: [Gene]
    shorterPadded = replicate lengthDiff CrossoverPadding <> shorter

-- |Takes the PushArgs for the evolutionary run and a singular plushy.
-- Returns the added onto plushy. Returns the the passed plushy with
-- new instructions possibly added before or after each existing instruction.
uniformAddition :: PushArgs -> [Gene] -> [Gene]
uniformAddition pushArgs plushy = undefined

newIndividual :: PushArgs -> [Individual] -> Individual
newIndividual = error "Implement this later"
