{-# LANGUAGE TemplateHaskell #-}

module HushGP.GP.PushData where

import HushGP.State
import Control.Lens

data PushData = PushData {
  _inputData :: [Gene],
  _outputData :: Gene,
  _downsampleIndex :: Maybe Int,
  _caseDistances :: Maybe [Double]
}

-- |Utility function: Sets the index of the passed training data.
makeIndexedTrainingData :: [PushData] -> [PushData]
makeIndexedTrainingData oldData = zipWith (\dat idx -> dat{_downsampleIndex = Just idx}) oldData [0..]

$(makeLenses ''PushData)
