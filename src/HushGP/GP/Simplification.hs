module HushGP.GP.Simplification where

import Control.Monad
import HushGP.State
import HushGP.GP.PushArgs

-- | Simplifies a Plushy by randomly deleting instructions and seeing how it impacts
-- performance. Removes genes that have zero to negative performance impact.
autoSimplifyPlushy :: PushArgs -> [Gene] -> IO [Gene]
autoSimplifyPlushy pushArgs@PushArgs{simplificationVerbose = simpVerbose, errorFunction = eFunc, trainingData = tData} plushy = do
  when simpVerbose (print ("simplification start plushy length: " <> show (length plushy)))
  autoSimplifyPlushy' pushArgs (eFunc pushArgs tData plushy) 0 plushy

autoSimplifyPlushy' :: PushArgs -> [Double] -> Int -> [Gene] -> IO [Gene]
autoSimplifyPlushy' pushArgs@PushArgs{simplificationSteps = simpSteps} initialErrors step plushy
  | step < simpSteps = do
      newPlushy <- undefined
      undefined
  | otherwise = undefined
