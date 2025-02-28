-- | The main file containing information about the GP loop and various population transformation functions.
module HushGP.GP where

import Control.Monad
import Control.Parallel.Strategies
import Data.List (sort, uncons)
import HushGP.GP.Downsample
import HushGP.GP.PushArgs
import HushGP.GP.PushData
import HushGP.GP.Variation
import HushGP.Genome

-- import Debug.Trace (trace, traceStack)

-- | Using a PushArgs object, generates a population of the specified size with the
-- specified instructions in parallel.
generatePopulation :: PushArgs -> IO [Individual]
generatePopulation pushArgs@(PushArgs {populationSize = popSize}) = do
  pop <- replicateM popSize (makeRandomIndividual pushArgs)
  return (pop `using` evalList rpar) -- Does this work? Need to test this with the HEC viewing tool.

-- | Evaluates a population of plushies with the error function passed in via PushArgs and sorts them.
-- TODO: Need to make this runnable in parallel too.
evaluatePopulation :: PushArgs -> [PushData] -> [Individual] -> [Individual]
evaluatePopulation pushArgs passedTrainingData population = sort $ zipWith updateIndividual (map (errorFunction pushArgs pushArgs passedTrainingData . plushy) population) population

-- | A helper function used in evaluatePopulation. Takes a [Double] as the error scores and an individual.
-- Updates the error fields in an individual, and returns it.
updateIndividual :: [Double] -> Individual -> Individual
updateIndividual errors ind = ind {totalFitness = Just (sum errors), fitnessCases = Just errors}

-- | The start of the gp loop. Generates the population and then calls
-- gpLoop' with modifications to the variables if needed.
gpLoop :: PushArgs -> IO ()
gpLoop pushArgs@(PushArgs {trainingData = tData}) = do
  unEvaledPopulation <- generatePopulation pushArgs
  let indexedTrainingData = assignIndiciesToData tData
  gpLoop' pushArgs 0 0 unEvaledPopulation indexedTrainingData

-- | The guts of the GP loop. Where the work gets done after the initialization happens
-- in the main gpLoop function. The first Int holds the generation count. The second Int
-- holds the evaluation count. The list of Individuals is the population. The last parameter is
-- the training data (possibly downsampled).
gpLoop' :: PushArgs -> Int -> Int -> [Individual] -> [PushData] -> IO ()
gpLoop' pushArgs generation evaluations population indexedTrainingData = do
  print "Put information about each generation here."
  when bestIndPassesDownsample $ print $ "Semi Success Generation: " <> show generation
  let nextAction
        | (bestIndPassesDownsample &&
          ((case totalFitness (updateIndividual (errorFunction epsilonPushArgs epsilonPushArgs indexedTrainingData (plushy bestInd)) bestInd) of (Just x) -> x; _ -> error "Error: Best downsample individual has no fitness!")
          <= solutionErrorThreshold epsilonPushArgs)) || (not (enableDownsampling epsilonPushArgs) && ((case totalFitness bestInd of (Just x) -> x; _ -> error "error: Best non-downsample individual has no fitness!") <= solutionErrorThreshold epsilonPushArgs)) =
            do
              print $ "Successful generation: " <> show generation
              print $ "Successful plushy: " <> show (plushy bestInd)
              print $ "Successful program: " <> show (plushyToPush $ plushy bestInd)
              when (useSimplification epsilonPushArgs) $
                do
                  let simplifiedPlushy = undefined -- TODO: simplification later
                  print "Total test error simplified: " <> undefined -- Implement later
                  print $ "Simplified plushy: " <> undefined -- show simplifiedPlushy
                  print $ "Simplified program: " <> undefined -- show plushyToPush simplifiedPlushy
        | (not (enableDownsampling epsilonPushArgs) && (generation >= maxGenerations epsilonPushArgs))
            || (enableDownsampling epsilonPushArgs && (evaluations >= (maxGenerations epsilonPushArgs * length population * length indexedTrainingData))) =
            print $ "Best individual: " <> show (plushy bestInd)
        | otherwise =
            gpLoop'
              pushArgs
              (succ generation)
              ( evaluations
                  + (populationSize pushArgs * length (trainingData pushArgs))
                  + (if generation `mod` downsampleParentsGens pushArgs == 0 then length parentReps * (length indexedTrainingData - length (trainingData pushArgs)) else 0)
                  + (if bestIndPassesDownsample then length indexedTrainingData - length (trainingData pushArgs) else 0)
              )
              ( if elitism pushArgs
                  then bestInd : replicate (populationSize epsilonPushArgs - 1) (newIndividual epsilonPushArgs evaledPop)
                  else replicate (populationSize epsilonPushArgs) (newIndividual epsilonPushArgs evaledPop)
              )
              ( if enableDownsampling pushArgs && ((generation `mod` downsampleParentsGens pushArgs) == 0)
                  then updateCaseDistances repEvaluatedPop indexedTrainingData indexedTrainingData (informedDownsamplingType pushArgs) (solutionErrorThreshold pushArgs / fromIntegral @Int @Double (length indexedTrainingData))
                  else indexedTrainingData
              )
  nextAction
  where
    -- \| This will have downsampling functionality added later.
    parentReps :: [Individual]
    parentReps = []
    -- \| This will have downsampling functionality added later.
    repEvaluatedPop :: [Individual]
    repEvaluatedPop = []
    evaledPop :: [Individual]
    evaledPop = evaluatePopulation pushArgs indexedTrainingData population
    bestInd :: Individual
    bestInd = case uncons evaledPop of Just (x, _) -> x; _ -> error "Error: Population is empty!"
    bestIndPassesDownsample :: Bool
    bestIndPassesDownsample = False -- TODO: fix this later
    epsilonPushArgs :: PushArgs
    epsilonPushArgs = pushArgs {epsilons = Nothing} -- TODO: And this
