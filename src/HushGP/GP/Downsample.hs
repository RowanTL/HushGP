module HushGP.GP.Downsample where

import System.Random.Shuffle
import System.Random
import Data.List
import HushGP.Genome
import HushGP.GP.PushData
import HushGP.GP.PushArgs
import HushGP.Tools.Metrics
import HushGP.Instructions.Utility

-- |Sets the index of the passed training data.
assignIndiciesToData :: [PushData] -> [PushData]
assignIndiciesToData oldData = zipWith (\dat idx -> dat{_downsampleIndex = Just idx}) oldData [0..]

-- |Initializes cases distances for passed training data.
initializeCaseDistances :: PushArgs -> [PushData]
initializeCaseDistances (PushArgs {trainingData = tData, populationSize = popSize}) = [ dat{_caseDistances = Just (replicate (length tData) (fromIntegral @Int @Double popSize))} | dat <- tData ]

-- |Updates the cases distances when downsampling
updateCaseDistances :: [Individual] -> [PushData] -> [PushData] -> String -> Double -> [PushData]
updateCaseDistances evaledPop downsampleData trainingData informedDownsamplingType solutionThreshold = undefined

-- |Draws a random amount of data points from a passed list of data points.
selectDownsampleRandom :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleRandom (PushArgs {downsampleRate = dsRate}) pushData = take (floor (dsRate * fromIntegral @Int @Float (length pushData))) . shuffle' pushData (length pushData) <$> initStdGen

-- |Selects a downsample that has it's cases maximally far away by sequentially
-- adding cases to the downsample that have their closest case maximally far away.
selectDownsampleMaxmin :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleMaxmin (PushArgs {downsampleRate = dsRate}) pushData = do
  shuffledCases <- shuffle' pushData (length pushData) <$> initStdGen
  let goalSize = floor @Float @Int (dsRate * (fromIntegral @Int @Float $ length pushData))
  selectDownsampleMaxmin'
    (case uncons shuffledCases of (Just (x, _)) -> [x]; _ -> error "error: shuffledCases empty!")
    (drop 1 shuffledCases)
    goalSize

-- |The main loop of selectDownsampleMaxmin. This is where most of calculation happens.
-- When called from selectDownsampleMaxmin: The first [PushData] holds the head of the
-- original pushData wrapped in a list, the second [PushData] holds the rest of the list
-- without the aformentioned head. The Int is the goal size derived from the downsample rate
-- and the length of the original [pushData].
selectDownsampleMaxmin' :: [PushData] -> [PushData] -> Int -> IO [PushData]
selectDownsampleMaxmin' newDownsample casesToPickFrom goalSize
  | length newDownsample >= goalSize = pure newDownsample
  | otherwise = do
      let newDistances = map extractDistance newDownsample
      let minCaseDistances = minOfColumns (map (\distList -> filterByIndex distList (map extractIndex casesToPickFrom)) newDistances)
      selectedCaseIndex <- argMax minCaseDistances
      stdGen <- initStdGen
      selectDownsampleMaxmin'
        ((casesToPickFrom !! selectedCaseIndex) : newDownsample)
        (shuffle' (deleteAt selectedCaseIndex casesToPickFrom) (length casesToPickFrom - 1) stdGen)
        goalSize

-- |selects a downsample that has it's cases maximally far away by sequentially 
-- adding cases to the downsample that have their closest case maximally far away
-- automatically stops when the maximum minimum distance is below delta
selectDownsampleMaxminAdaptive :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleMaxminAdaptive (PushArgs {caseDelta = cDelta}) pushData = do
  shuffledCases <- shuffle' pushData (length pushData) <$> initStdGen
  selectDownsampleMaxminAdaptive'
    (case uncons shuffledCases of (Just (x, _)) -> [x]; _ -> error "error: shuffledCases empty!")
    (drop 1 shuffledCases)
    cDelta

-- |The main loop of selectDownsampleMaxmin. This is where most of calculation happens.
-- When called from selectDownsampleMaxmin: The first [PushData] holds the head of the
-- original pushData wrapped in a list, the second [PushData] holds the rest of the list
-- without the aformentioned head. The Int is the caseDelta derived from the downsample rate
-- and the length of the original [pushData].
selectDownsampleMaxminAdaptive' :: [PushData] -> [PushData] -> Double -> IO [PushData]
selectDownsampleMaxminAdaptive' newDownsample casesToPickFrom cDelta = do
      let newDistances = map extractDistance newDownsample
      let minCaseDistances = minOfColumns (map (\distList -> filterByIndex distList (map extractIndex casesToPickFrom)) newDistances)
      selectedCaseIndex <- argMax minCaseDistances
      stdGen <- initStdGen
      if null casesToPickFrom || (maximum minCaseDistances <= cDelta)
      then pure newDownsample
      else selectDownsampleMaxminAdaptive'
            ((casesToPickFrom !! selectedCaseIndex) : newDownsample)
            (shuffle' (deleteAt selectedCaseIndex casesToPickFrom) (length casesToPickFrom - 1) stdGen)
            cDelta
