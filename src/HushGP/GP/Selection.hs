module HushGP.GP.Selection where

import Data.List
import System.Random
import System.Random.Shuffle
import HushGP.GP.PushArgs
import HushGP.GP.Individual
import HushGP.Utility

-- | Tournament selection based off tournament size from PushArgs and a population.
-- Takes the individual with the lowest total error in the tournament.
tournamentSelection :: PushArgs -> [Individual] -> IO Individual
tournamentSelection PushArgs{tournamentSize = tSize} pop = do
  shuffledPop <- shuffle' pop (length pop) <$> initStdGen
  let tournSet = take tSize shuffledPop
  pure $ minimum tournSet

-- |Selects an individual from the population using lexicase selection.
-- Lexicase parent selection filters the population by considering one random training case at a time,
-- eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
-- until a single individual remains. This is the top level function.
lexicaseSelection :: PushArgs -> [Individual] -> IO Individual
lexicaseSelection PushArgs{initialCases = iCases} pop = do
  startCases <- maybe (shuffle' [0..lehp] lehp <$> initStdGen) (pure @IO) iCases
  survivors <- mapM randElem (groupBy (\x y -> fitnessCases x == fitnessCases y) pop)
  lexicaseSelection' survivors startCases startCases
  where
    lehp :: Int -- length of the extracted fitness cases of the head of the passed population.
    lehp = length $ extractFitnessCases $
      case uncons pop of
        Just (x, _) -> x
        _ -> error "Error: Population in lexicaseSelection cannot be empty!"

lexicaseSelection' :: [Individual] -> [Int] -> [Int] -> IO Individual
lexicaseSelection' survivors cases initialCases =
  if null cases || null (drop 1 survivors)
  then (\ind -> ind{selectionCases = Just initialCases}) <$> randElem survivors
  else lexicaseSelection' ()
  where
    minErrorForCase :: Double
    minErrorForCase = minimum $ map ((\x -> x !! case uncons cases of Just (y, _) -> y; _ -> error "Error: cases is empty!") . extractFitnessCases) survivors
