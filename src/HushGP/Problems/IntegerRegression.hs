module HushGP.Problems.IntegerRegression where

import Data.List.Split
import Data.List
import Data.Map qualified as Map
import HushGP.State
import HushGP.Instructions
import HushGP.GP.PushArgs
import HushGP.GP.PushData
import HushGP.Genome
import HushGP.Push
import HushGP.Instructions.Utility
import HushGP.GP

testPlushy :: [Gene]
testPlushy = [
    PlaceInput 0,
    GeneInt 0,
    StateFunc (instructionIntAdd, "instructionIntAdd")
    -- GeneFloat 3.2
  ]

-- | The target function for this run. The function the gp
-- is trying to evolve.
targetFunction :: Integer -> Integer
targetFunction x = (x * x * x) + (2 * x)

-- | The training data for the model.
trainData :: [PushData]
trainData = map (\num -> PushData {
      inputData = [GeneInt num],
      outputData = (GeneInt . targetFunction) num,
      downsampleIndex = Nothing,
      caseDistances = Nothing})
    [-10..10]

-- | The testing data for the model.
testData :: [PushData]
-- testData = (chunksOf 1 $ map GeneInt $ [-20..(-11)] <> [11..21], map (GeneInt . targetFunction) ([-20..(-11)] <> [11..21]))
testData = map (\num -> PushData {
      inputData = [GeneInt num],
      outputData = (GeneInt . targetFunction) num,
      downsampleIndex = Nothing,
      caseDistances = Nothing})
    [-20..(-11)] <> [11..21]

-- | The instructions used in the evolutionary run.
runInstructions :: [Gene]
runInstructions =
  [
    PlaceInput 0,
    Close,
    GeneInt 1,
    GeneInt 0
  ]
  <> allIntInstructions

-- | Takes the head of the stack and returns it. If there is no head, returns an
-- error amount.
errorHead :: [Integer] -> Integer
errorHead xs =
  case uncons xs of
    Just (x, _) -> x
    _ -> 100000000 -- Make this a variable for later?

-- | Loads a plushy and a list of genes into the input state.
loadState :: [Gene] -> [Gene] -> State
loadState plushy vals = 
  (loadProgram (plushyToPush plushy) emptyState){_input = Map.fromList (zip [0..] vals)}

-- | The error function for a single set of inputs and outputs.
intErrorFunction :: PushArgs -> [PushData] -> [Gene] -> [Double]
intErrorFunction _args pushData plushy =
  map abs $ zipWith (-) (map ((fromIntegral @Integer @Double . (errorHead . _int) . interpretExec) . loadState plushy) pushData) (map (fromIntegral @Integer @Double . extractGeneInt) outputData)

intPushArgs :: PushArgs
intPushArgs = defaultPushArgs
  {
    instructionList = runInstructions,
    errorFunction = intErrorFunction,
    trainingData = trainData,
    testingData = testData,
    maxGenerations = 300,
    populationSize = 1000,
    maxInitialPlushySize = 100,
    stepLimit = 200,
    parentSelectionAlgo = "lexicase",
    tournamentSize = 5,
    umadRate = 0.1,
    variation = Map.fromList [("umad", 1.0), ("crossover", 0.0)],
    elitism = False,
    enableDownsampling = False
  }

main :: IO ()
main = gpLoop intPushArgs
