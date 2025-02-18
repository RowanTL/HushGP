module HushGP.GP where

import HushGP.State
import HushGP.Genome
import HushGP.GP.PushArgs
import Control.Monad
import Control.Parallel.Strategies
-- import Debug.Trace (trace, traceStack)

-- | Using a PushArgs object, generates a population of the specified size with the
-- specified instructions in parallel.
generatePopulation :: PushArgs -> [Individual]
generatePopulation pushArgs =
  replicate (populationSize pushArgs) (makeRandomIndividual pushArgs) `using` rpar

evaluatePopulation :: PushArgs -> [Individual] -> IO [Individual]
evaluatePopulation pushArgs population = map (fmap (errorFunction pushArgs pushArgs (trainingData pushArgs)) . plushy) population

-- | The start of the gp loop. TODO: Make this more accurate later.
gpLoop :: PushArgs -> IO ()
gpLoop pushArgs = do
  let unEvaledPopulation =  generatePopulation pushArgs
  print "gamer"
