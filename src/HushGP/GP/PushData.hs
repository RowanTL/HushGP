module HushGP.GP.PushData where

import HushGP.State

data PushData = PushData {
  inputData :: [Gene],
  outputData :: Gene,
  downsampleIndex :: Maybe Int,
  caseDistances :: Maybe [Double]
}

-- |Utility function: Sets the index of the passed training data.
makeIndexedTrainingData :: [PushData] -> [PushData]
makeIndexedTrainingData oldData = zipWith (\dat idx -> dat{downsampleIndex = Just idx}) oldData [0..]
