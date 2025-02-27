module HushGP.GP.Downsample where

import System.Random.Shuffle
import System.Random
import HushGP.Genome
import HushGP.GP.PushData
import HushGP.GP.PushArgs

-- |Sets the index of the passed training data.
assignIndiciesToData :: [PushData] -> [PushData]
assignIndiciesToData oldData = zipWith (\dat idx -> dat{_downsampleIndex = Just idx}) oldData [0..]

-- |Initializes cases distances for passed training data.
initializeCaseDistances :: PushArgs -> [PushData]
initializeCaseDistances pushArgs = [ dat{_caseDistances = Just (replicate (length $ trainingData pushArgs) (fromIntegral @Int @Double $ populationSize pushArgs))} | dat <- trainingData pushArgs ]

-- |Updates the cases distances when downsampling
updateCaseDistances :: [Individual] -> [PushData] -> [PushData] -> String -> Double -> [PushData]
updateCaseDistances evaledPop downsampleData trainingData informedDownsamplingType solutionThreshold = undefined

-- |Draws a random amount of data points from a passed list of data points.
selectDownsampleRandom :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleRandom pushArgs pushData = take (floor (downsampleRate pushArgs * fromIntegral @Int @Float (length pushData))) . shuffle' pushData (length pushData) <$> initStdGen

selectDownsampleMaxmin :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleMaxmin pushArgs@(PushArgs {downsampleRate = dsRate}) pushData = do
  shuffledCases <- shuffle' pushData (length pushData) <$> initStdGen
  
