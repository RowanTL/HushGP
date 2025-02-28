{-# LANGUAGE TemplateHaskell #-}

module HushGP.GP.PushData where

import HushGP.State
import Control.Lens

data PushData = PushData {
  _inputData :: [Gene],
  _outputData :: Gene,
  _downsampleIndex :: Maybe Int,
  _caseDistances :: Maybe [Int]
} deriving (Show)

-- |Extracts the case distances from a PushData object. Errors if the
-- _caseDistances list is Nothing.
extractDistance :: PushData -> [Int]
extractDistance PushData{_caseDistances = Nothing} = error "Error: Case distances are empty!. This should never happen"
extractDistance PushData{_caseDistances = Just xs} = xs

-- |Extracts the downsample index from a PushData object. Errors if the
-- _downsampleIndex is Nothing.
extractIndex :: PushData -> Int
extractIndex PushData{_downsampleIndex = Nothing} = error "Error: Case distances are empty!. This should never happen"
extractIndex PushData{_downsampleIndex = Just x} = x

-- |Filters a list by another list of indicies.
filterByIndex :: [a] -> [Int] -> [a]
filterByIndex origList = map (origList !!)

$(makeLenses ''PushData)
