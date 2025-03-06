-- | The main file containing information about the GP loop and various population transformation functions.
module HushGP.GP where

import Control.Monad
import Control.Parallel.Strategies
import Data.List (sort, uncons)
import HushGP.GP.Downsample
import HushGP.GP.Individual
import HushGP.GP.PushArgs
import HushGP.GP.PushData
import HushGP.GP.Variation
import HushGP.GP.Selection
import HushGP.GP.Simplification
import HushGP.Genome
import System.Random
import System.Random.Shuffle

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
  let indexedTrainingData = assignIndicesToData tData
  gpLoop' pushArgs 0 0 unEvaledPopulation indexedTrainingData

-- | The guts of the GP loop. Where the work gets done after the initialization happens
-- in the main gpLoop function. The first Int holds the generation count. The second Int
-- holds the evaluation count. The list of Individuals is the population. The last parameter is
-- the training data (possibly downsampled).
gpLoop' :: PushArgs -> Int -> Int -> [Individual] -> [PushData] -> IO ()
gpLoop'
  pushArgs@(PushArgs {enableDownsampling = enableDS, solutionErrorThreshold = seThresh, downsampleParentsGens = dsParentGens, downsampleParentRate = dsParentRate, trainingData = trData, elitism = isElite, populationSize = popSize, useSimplification = useSimp, errorFunction = errorFunc, maxGenerations = maxGens, testingData = teData})
  generation
  evaluations
  population
  indexedTrainingData = do
    print "Put information about each generation here."
    when bestIndPassesDownsample $ print $ "Semi Success Generation: " <> show generation
    parentReps <- do
      shuffledParents <- shuffle' population (length population) <$> initStdGen
      if enableDS && (generation `mod` dsParentGens == 0)
        then pure $ take (floor @Float (dsParentRate * (fromIntegral @Int @Float $ length population))) shuffledParents
        else pure []
    let nextAction
          | ( bestIndPassesDownsample
                && ( (case totalFitness (updateIndividual (errorFunc epsilonPushArgs indexedTrainingData (plushy bestInd)) bestInd) of (Just x) -> x; _ -> error "Error: Best downsample individual has no fitness!")
                       <= solutionErrorThreshold epsilonPushArgs
                   )
            )
              || (not enableDS && ((case totalFitness bestInd of (Just x) -> x; _ -> error "error: Best non-downsample individual has no fitness!") <= seThresh)) =
              do
                print $ "Successful generation: " <> show generation
                print $ "Successful plushy: " <> show (plushy bestInd)
                print $ "Successful program: " <> show (plushyToPush pushArgs (plushy bestInd))
                print $ "Total test error: " <> show (errorFunc epsilonPushArgs teData (plushy bestInd))
                when useSimp $
                  do
                    simplifiedPlushy <- autoSimplifyPlushy pushArgs (plushy bestInd)
                    print $ "Simplified plushy: " <> show simplifiedPlushy
                    print $ "Simplified program: " <> show (plushyToPush pushArgs simplifiedPlushy)
                    print $ "Total simplified test error: " <> show (errorFunc epsilonPushArgs teData simplifiedPlushy)
          | (not enableDS && (generation >= maxGens))
              || (enableDS && (evaluations >= (maxGens * length population * length indexedTrainingData))) =
              print $ "Best individual: " <> show (plushy bestInd)
          | otherwise = do
              newPop <- if isElite then replicateM (popSize - 1) (newIndividual epsilonPushArgs evaledPop) else replicateM popSize (newIndividual epsilonPushArgs evaledPop)
              gpLoop'
                pushArgs
                (succ generation)
                ( evaluations
                    + (populationSize pushArgs * length (trainingData pushArgs))
                    + (if generation `mod` downsampleParentsGens pushArgs == 0 then length parentReps * (length indexedTrainingData - length (trainingData pushArgs)) else 0)
                    + (if bestIndPassesDownsample then length indexedTrainingData - length trData else 0)
                )
                ( if isElite
                    then bestInd : newPop
                    else newPop
                )
                ( if enableDS && ((generation `mod` dsParentGens) == 0)
                    then updateCaseDistances repEvaluatedPop indexedTrainingData indexedTrainingData (informedDownsamplingType pushArgs) (seThresh / fromIntegral @Int @Double (length indexedTrainingData))
                    else indexedTrainingData
                )
    nextAction
    where
      -- \| This will have downsampling functionality added later.
      repEvaluatedPop :: [Individual]
      repEvaluatedPop =
        if enableDS
          then evaluatePopulation pushArgs indexedTrainingData population
          else []
      evaledPop :: [Individual]
      evaledPop = evaluatePopulation pushArgs trData population
      bestInd :: Individual
      bestInd = case uncons evaledPop of Just (x, _) -> x; _ -> error "Error: Population is empty!"
      bestIndPassesDownsample :: Bool
      bestIndPassesDownsample = enableDS && (extractTotalFitness bestInd <= seThresh)
      epsilonPushArgs :: PushArgs
      epsilonPushArgs = pushArgs {epsilons = Just $ epsilonList evaledPop}
