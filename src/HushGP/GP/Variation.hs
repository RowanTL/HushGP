module HushGP.GP.Variation where

import Data.List
import Control.Monad
import System.Random
import HushGP.State
import HushGP.GP.PushArgs
import HushGP.GP.Individual
import HushGP.Utility
import HushGP.Genome
import HushGP.GP.Selection

-- |Performs a uniform crossover on two parents and returns the child.
-- Padding is placed to left of the shorter genome.
crossover :: [Gene] -> [Gene] -> IO [Gene]
crossover plushyA plushyB = do
  filter (CrossoverPadding /=) <$> zipWithM (\short long -> randZeroToOne >>= (\num -> if num < 0.5 then pure short else pure long)) shorterPadded longer
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
  randNum <- randZeroToOne
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
  filter (CrossoverPadding /=) <$> zipWithM (\short long -> randZeroToOne >>= (\num -> if num < 0.5 then pure short else pure long)) shorterPadded longer
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
uniformAddition :: PushArgs -> [Gene] -> IO [Gene]
uniformAddition pushArgs plushy = uniformAddition' pushArgs plushy []

-- |Guts of uniform addition. Appends to the second [Gene] recursively until the first [Gene]
-- is empty. Ignores Gaps used for bmx if applicable.
uniformAddition' :: PushArgs -> [Gene] -> [Gene] -> IO [Gene]
uniformAddition' _ [] newPlushy = pure newPlushy
uniformAddition' pushArgs@PushArgs{instructionList = iList, umadRate = uRate} (old:oldList) !newList = do
  frontInstruction <- randomInstruction iList
  backInstruction <- randomInstruction iList
  frontZeroToOne <- randZeroToOne
  backZeroToOne <- randZeroToOne
  uniformAddition' pushArgs oldList (newList <> [frontInstruction | frontZeroToOne < uRate && not (isGap old)] <> [old] <> [backInstruction | backZeroToOne < uRate && not (isGap old)])

-- |Takes the PushArgs for the evolutionary run and a singular plushy.
-- Returns the replacement plushy. Returns the the passed plushy with
-- new instructions possibly replacing each existing instruction.
uniformReplacement :: PushArgs -> [Gene] -> IO [Gene]
uniformReplacement pushArgs plushy = uniformAddition' pushArgs plushy []

-- |Guts of uniform replacement. Appends to the second [Gene] recursively until the first [Gene]
-- is empty.
uniformReplacement' :: PushArgs -> [Gene] -> [Gene] -> IO [Gene]
uniformReplacement' _ [] newPlushy = pure newPlushy
uniformReplacement' pushArgs@PushArgs{instructionList = iList, replacementRate = rRate} (old:oldList) !newList = do
  randInstruction <- randomInstruction iList
  randDecimal <- randZeroToOne
  uniformReplacement' pushArgs oldList (newList <> if randDecimal < rRate then [randInstruction] else [old])

-- |Takes the PushArgs for the evolutionary run and a singular plushy.
-- Returns the deleted plushy. Returns the passed plushy with
-- instructions that were there possibly deleted. Ignores Gaps used for bmx if applicable.
uniformDeletion :: PushArgs -> [Gene] -> IO [Gene]
uniformDeletion PushArgs{umadRate = uRate} plushy =
  if uRate == 0
  then pure plushy
  else uniformDeletion' plushy [] adjustedRate
  where
    adjustedRate :: Double
    adjustedRate = 1 / (1 + (1 / uRate))

-- |Internals for uniform deletion. The Double is the adjusted rate
-- calculated based on the original umad rate.
uniformDeletion' :: [Gene] -> [Gene] -> Double -> IO [Gene]
uniformDeletion' [] newPlushy _ = pure newPlushy
uniformDeletion' (old:oldList) !newList adjustRate = do
  randDecimal <- randZeroToOne
  uniformDeletion' oldList (newList <> [old | randDecimal < adjustRate]) adjustRate

-- |Creates a new individual based on the probabilities of the desired
-- crossover methods.
newIndividual :: PushArgs -> [Individual] -> IO Individual
newIndividual pushArgs@PushArgs{variation = var, umadRate = uRate} population = do
  randOp <- randomOperation var 0.0
  case randOp of
    "reproduction" -> selectParent pushArgs population
    "crossover" -> do
      parent0 <- selectParent pushArgs population
      parent1 <- selectParent pushArgs population
      childPlushy <- crossover (plushy parent0) (plushy parent1)
      pure $ postVariationInd childPlushy
    "tailAlignedCrossover" -> do
      parent0 <- selectParent pushArgs population
      parent1 <- selectParent pushArgs population
      childPlushy <- tailAlignedCrossover (plushy parent0) (plushy parent1)
      pure $ postVariationInd childPlushy
    "umad" -> do
      parent <- selectParent pushArgs population
      childPlushy <- uniformAddition pushArgs (plushy parent) >>= uniformDeletion pushArgs
      pure $ postVariationInd childPlushy
    "alternation" -> do
      parent0 <- selectParent pushArgs population
      parent1 <- selectParent pushArgs population
      childPlushy <- alternation pushArgs (plushy parent0) (plushy parent1)
      pure $ postVariationInd childPlushy
    "rumad" -> do -- Responsive umad, deletion rate from computed amount of additions.
      parent <- selectParent pushArgs population
      addedChildPlushy <- uniformAddition pushArgs (plushy parent)
      let effectiveAdditionRate = fromIntegral @Int @Double (length addedChildPlushy - length (plushy parent)) / fromIntegral @Int @Double (length (plushy parent))
      finalChild <- uniformDeletion pushArgs{umadRate = effectiveAdditionRate} addedChildPlushy
      pure $ postVariationInd finalChild
    "vumad" -> do -- variable umad, umad rate chosen randomly from [0, umadRate]
      rate <- fst . uniformR (0.0 :: Double, uRate) <$> initStdGen
      parent <- selectParent pushArgs population
      addedChildPlushy <- uniformAddition pushArgs{umadRate = rate} (plushy parent)
      deletedChildPlushy <- uniformDeletion pushArgs{umadRate = rate} addedChildPlushy
      pure $ postVariationInd deletedChildPlushy
    "uniformAddition" -> do
      parent <- selectParent pushArgs population
      childPlushy <- uniformAddition pushArgs (plushy parent)
      pure $ postVariationInd childPlushy
    "uniformReplacement" -> do
      parent <- selectParent pushArgs population
      childPlushy <- uniformReplacement pushArgs (plushy parent)
      pure $ postVariationInd childPlushy
    "uniformDeletion" -> do
      parent <- selectParent pushArgs population
      childPlushy <- uniformDeletion pushArgs (plushy parent)
      pure $ postVariationInd childPlushy
    _ -> error ("Error: No match for selection operation: " <> randOp)
  where
  randDecimal :: IO Double
  randDecimal = randZeroToOne
  randomOperation :: [(String, Double)] -> Double -> IO String
  randomOperation operations acc = do
    randD <- randDecimal
    let nextAction
          | null operations = pure "reproduction"
          | acc + tempProb >= randD = pure tempOp
          | otherwise = randomOperation (drop 1 operations) (tempProb + acc)
    nextAction
    where
    (tempOp,tempProb) = case uncons operations of Just (x, _) -> x; _ -> error "Error: operations cannot be empty!"
      

