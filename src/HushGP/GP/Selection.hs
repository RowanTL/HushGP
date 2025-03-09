module HushGP.GP.Selection where

import Numeric.Statistics.Median (medianFast)
import Data.List
import Data.Maybe
import System.Random
import HushGP.GP.PushArgs
import HushGP.GP.Individual
import HushGP.Utility

-- | Tournament selection based off tournament size from PushArgs and a population.
-- Takes the individual with the lowest total error in the tournament.
tournamentSelection :: PushArgs -> [Individual] -> IO Individual
tournamentSelection PushArgs{tournamentSize = tSize} pop = do
  shuffledPop <- fst. uniformShuffleList pop <$> initStdGen
  let tournSet = take tSize shuffledPop
  pure $ minimum tournSet

-- |Selects an individual from the population using lexicase selection.
-- Lexicase parent selection filters the population by considering one random training case at a time,
-- eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
-- until a single individual remains. This is the top level function.
lexicaseSelection :: PushArgs -> [Individual] -> IO Individual
lexicaseSelection PushArgs{initialCases = iCases} pop = do
  startCases <- maybe (fst . uniformShuffleList [0..lehp] <$> initStdGen) (pure @IO) iCases
  survivors <- mapM randElem (groupBy (\x y -> fitnessCases x == fitnessCases y) pop)
  lexicaseSelection' survivors startCases startCases
  where
    lehp :: Int -- length of the extracted fitness cases of the head of the passed population.
    lehp = length $ extractFitnessCases $
      case uncons pop of
        Just (x, _) -> x
        _ -> error "Error: Population in lexicaseSelection cannot be empty!"

-- |The internals of lexicaseSelection selection. Loops for each of the survivors each lexicase loop.
lexicaseSelection' :: [Individual] -> [Int] -> [Int] -> IO Individual
lexicaseSelection' survivors cases initialCases =
  if null cases || null (drop 1 survivors)
  then (\ind -> ind{selectionCases = Just initialCases}) <$> randElem survivors
  else lexicaseSelection'
    (filter (\ind -> (extractFitnessCases ind !! case uncons cases of Just (x, _) -> x; _ -> error "Error: cases list is empty!") == minErrorForCase) survivors)
    (drop 1 cases)
    initialCases
  where
    minErrorForCase :: Double
    minErrorForCase = minimum $ map ((\x -> x !! case uncons cases of Just (y, _) -> y; _ -> error "Error: cases is empty!") . extractFitnessCases) survivors

-- |Calculates the median absolute deviation for a list of fractional numbers.
medianAbsoluteDeviation :: forall a. (Fractional a, Ord a) => [a] -> a
medianAbsoluteDeviation xs = medianFast $ map (\x -> abs (x - medianVal)) xs
  where
    medianVal :: a
    medianVal = medianFast xs

-- | Calculates the epsilon list of a given population. Used in epsilon lexicase selection.
epsilonList :: [Individual] -> [Double]
epsilonList pop = epsilonList' [] 0 errorList errorLength
  where
  errorList :: [[Double]]
  errorList = map extractFitnessCases pop
  errorLength :: Int
  errorLength = length $ extractFitnessCases (case uncons pop of Just (x, _) -> x; _ -> error "Error: pop is empty in epsilonList!")

-- | Internals for the epsilonList function.
epsilonList' :: [Double] -> Int -> [[Double]] -> Int -> [Double]
epsilonList' epsilons index errorList errorLength =
  if index == errorLength
  then epsilons
  else epsilonList' (medianAbsoluteDeviation (map (!! index) errorList) : epsilons) (succ index) errorList errorLength

-- |Selects an individual from the population using epsilon-lexicase selection.
-- Epsilon lexicase selection follows the same process as lexicase selection except,
-- for a test case, only individuals with an error outside of a predefined epsilon are filtered.
epsilonLexicaseSelection :: PushArgs -> [Individual] -> IO Individual
epsilonLexicaseSelection PushArgs{epsilons = eps} pop = do
  startCases <- fst . uniformShuffleList [0..lehp] <$> initStdGen
  epsilonLexicaseSelection' (fromMaybe (error "Error: epsilons list is empty!") eps) pop startCases
  where
    lehp :: Int -- length of the extracted fitness cases of the head of the passed population.
    lehp = length $ extractFitnessCases $
      case uncons pop of
        Just (x, _) -> x
        _ -> error "Error: Population in epsilonLexicaseSelection cannot be empty!"

-- |Internals for epsilon lexicase selection.
epsilonLexicaseSelection' :: [Double] -> [Individual] -> [Int] -> IO Individual
epsilonLexicaseSelection' eps survivors cases =
  if null cases || null (drop 1 survivors)
  then randElem survivors  
  else epsilonLexicaseSelection' eps (filter (\x -> (abs (extractFitnessCases x !! headCases cases) - minErrorForCase) <= epsilon) survivors) (drop 1 cases)
  where
    minErrorForCase :: Double
    minErrorForCase = minimum $ map ((\x -> x !! headCases cases) . extractFitnessCases) survivors
    epsilon :: Double
    epsilon = eps !! headCases cases

-- |Select the selection method the user specified in the passed PushArgs.
selectParent :: PushArgs -> [Individual] -> IO Individual
selectParent pushArgs@PushArgs{parentSelectionAlgo = selAlgo} pop =
  case selAlgo of
    "tournament" -> tournamentSelection pushArgs pop
    "lexicase" -> lexicaseSelection pushArgs pop
    "epsilonLexicase" -> epsilonLexicaseSelection pushArgs pop
    _ -> error "Error: selection strategy not found!"
