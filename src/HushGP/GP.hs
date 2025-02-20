-- | The main file containing information about the GP loop and various population transformation functions.
module HushGP.GP where

import HushGP.Genome
import HushGP.GP.PushArgs
import Control.Parallel.Strategies
import Control.Monad
import Data.List (sort)
-- import Debug.Trace (trace, traceStack)

-- | Using a PushArgs object, generates a population of the specified size with the
-- specified instructions in parallel.
generatePopulation :: PushArgs -> IO [Individual]
generatePopulation pushArgs = do
  pop <- replicateM (populationSize pushArgs) (makeRandomIndividual pushArgs)
  return (pop `using` evalList rpar) -- Does this work? Need to test this with the HEC viewing tool.

-- | Evaluates a population of plushies with the error function passed in via PushArgs and sorts them.
-- TODO: Need to make this runnable in parallel too.
evaluatePopulation :: PushArgs -> [Individual] -> [Individual]
evaluatePopulation pushArgs population = sort $ zipWith updateIndividual (map (errorFunction pushArgs pushArgs (trainingData pushArgs) . plushy) population) population

-- | A helper function used in evaluatePopulation. Takes a [Double] as the error scores and an individual.
-- Updates the error fields in an individual, and returns it.
updateIndividual :: [Double] -> Individual -> Individual
updateIndividual errors ind = ind{totalFitness = Just (sum errors), fitnessCases = Just errors}

-- | The start of the gp loop. TODO: Make this more accurate later.
gpLoop :: PushArgs -> IO ()
gpLoop pushArgs = do
  unEvaledPopulation <- generatePopulation pushArgs
  let evaledPop = evaluatePopulation pushArgs unEvaledPopulation
  print evaledPop

-- | The guts of the GP loop. Where the work gets done after the initialization happens
-- in the main gpLoop function.
-- gpLoop' 
