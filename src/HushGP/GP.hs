module HushGP.GP where

import HushGP.State
import HushGP.Genome
import HushGP.GP.PushArgs
-- import Debug.Trace (trace, traceStack)

-- generatePopulation :: PushArgs -> [Gene] -> IO [[Gene]]
-- generatePopulation pushArgs instructions = do
  -- randomPop <- makeRandomPlushy pushArgs
  -- replicate (populationSize pushArgs) (makeRandomPlushy pushArgs)

gpLoop :: PushArgs -> IO ()
gpLoop = undefined
