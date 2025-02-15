module HushGP.Problems.IntegerRegression where

import HushGP.State
import HushGP.Instructions
import Data.List.Split
import HushGP.GP.PushArgs
import HushGP.Genome
import HushGP.Push
import Data.Map qualified as Map

-- | The target function for this run. The function the gp
-- is trying to evolve.
targetFunction :: Integer -> Integer
targetFunction x = (x * x * x) + (2 * x)

-- | The training data for the model.
trainData :: ([[Gene]], [Gene])
trainData = (chunksOf 1 $ map GeneInt [-10..10], map (GeneInt . targetFunction) [-10..11])

-- | The testing data for the model.
testData :: ([[Gene]], [Gene])
testData = (chunksOf 1 $ map GeneInt $ [-20..(-11)] <> [11..21], map (GeneInt . targetFunction) ([-20..(-11)] <> [11..21]))

-- | The instructions used to 
runInstructions :: [Gene]
runInstructions =
  [
    PlaceInput 0,
    Close,
    GeneInt 1,
    GeneInt 0
  ]
  <> allIntInstructions

-- |The error function for a single set of inputs and outputs.
intErrorFunction :: PushArgs -> ([Gene], Gene) -> [Gene] -> [Double]
intErrorFunction args (inputData, outputData) plushy =
  head $ _int $ interpretExec loadedState
  where
    loadedState :: State
    loadedState = (loadProgram (plushyToPush plushy) emptyState){_input = Map.fromList (zip [0..] inputData)}
