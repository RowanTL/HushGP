{-# LANGUAGE TemplateHaskell #-}

module HushGP.GP.PushData where

import HushGP.State
import Control.Lens

data PushData = PushData {
  _inputData :: [Gene],
  _outputData :: Gene,
  _downsampleIndex :: Maybe Int,
  _caseDistances :: Maybe [Double]
} deriving (Show)

$(makeLenses ''PushData)
