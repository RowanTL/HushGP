module HushGP.GP.PushData where

import HushGP.State

data PushData = PushData {
  inputData :: [Gene],
  outputData :: [Gene],
  downsampleIndex :: Int,
  caseDistances :: [Double]
}
