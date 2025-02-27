module HushGP.GP.Downsample where

import System.Random.Shuffle
import System.Random
import HushGP.Genome
import HushGP.GP.PushData
import HushGP.GP.PushArgs
import Data.List

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
selectDownsampleRandom pushArgs pushData = take (floor (downsampleRate pushArgs * fromIntegral @Int @Float (length pushData))) . shuffle' pushData (length pushData) <$> initStdGen

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
      minCaseDistances
  
