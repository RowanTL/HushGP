module HushGP.GP.Downsample where

import System.Random.Shuffle
import System.Random
import Data.List
import Data.Maybe
import HushGP.Genome
import HushGP.Utility
import HushGP.GP.PushData
import HushGP.GP.PushArgs
import HushGP.Tools.Metrics
import HushGP.Instructions.Utility

-- |Sets the index of the passed training data.
assignIndicesToData :: [PushData] -> [PushData]
assignIndicesToData oldData = zipWith (\dat idx -> dat{_downsampleIndex = Just idx}) oldData [0..]

-- |Initializes cases distances for passed training data.
initializeCaseDistances :: PushArgs -> [PushData]
initializeCaseDistances (PushArgs {trainingData = tData, populationSize = popSize}) = [ dat{_caseDistances = Just (replicate (length tData) (fromIntegral @Int @Double popSize))} | dat <- tData ]

-- |Draws a random amount of data points from a passed list of data points.
selectDownsampleRandom :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleRandom (PushArgs {downsampleRate = dsRate}) pushData = take (floor (dsRate * fromIntegral @Int @Float (length pushData))) . shuffle' pushData (length pushData) <$> initStdGen

-- |Selects a downsample that has it's cases maximally far away by sequentially
-- adding cases to the downsample that have their closest case maximally far away.
selectDownsampleMaxmin :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleMaxmin (PushArgs {downsampleRate = dsRate}) pushData = do
  shuffledCases <- shuffle' pushData (length pushData) <$> initStdGen
  let goalSize = floor @Float @Int (dsRate * (fromIntegral @Int @Float $ length pushData))
  selectDownsampleMaxmin'
    (case uncons shuffledCases of (Just (x, _)) -> [x]; _ -> error "error: shuffledCases empty!")
    (drop 1 shuffledCases)
    goalSize

-- |The main loop of selectDownsampleMaxmin. This is where most of calculation happens.
-- When called from selectDownsampleMaxmin: The first [PushData] holds the head of the
-- original pushData wrapped in a list, the second [PushData] holds the rest of the list
-- without the aformentioned head. The Int is the goal size derived from the downsample rate
-- and the length of the original [pushData].
selectDownsampleMaxmin' :: [PushData] -> [PushData] -> Int -> IO [PushData]
selectDownsampleMaxmin' newDownsample casesToPickFrom goalSize
  | length newDownsample >= goalSize = pure newDownsample
  | otherwise = do
      let newDistances = map extractDistance newDownsample
      let minCaseDistances = minOfColumns (map (\distList -> filterByIndex distList (map extractIndex casesToPickFrom)) newDistances)
      selectedCaseIndex <- argMax minCaseDistances
      stdGen <- initStdGen
      selectDownsampleMaxmin'
        ((casesToPickFrom !! selectedCaseIndex) : newDownsample)
        (shuffle' (deleteAt selectedCaseIndex casesToPickFrom) (length casesToPickFrom - 1) stdGen)
        goalSize

-- |selects a downsample that has it's cases maximally far away by sequentially 
-- adding cases to the downsample that have their closest case maximally far away
-- automatically stops when the maximum minimum distance is below delta
selectDownsampleMaxminAdaptive :: PushArgs -> [PushData] -> IO [PushData]
selectDownsampleMaxminAdaptive (PushArgs {caseDelta = cDelta}) pushData = do
  shuffledCases <- shuffle' pushData (length pushData) <$> initStdGen
  selectDownsampleMaxminAdaptive'
    (case uncons shuffledCases of (Just (x, _)) -> [x]; _ -> error "error: shuffledCases empty!")
    (drop 1 shuffledCases)
    cDelta

-- |The main loop of selectDownsampleMaxmin. This is where most of calculation happens.
-- When called from selectDownsampleMaxmin: The first [PushData] holds the head of the
-- original pushData wrapped in a list, the second [PushData] holds the rest of the list
-- without the aformentioned head. The Int is the caseDelta derived from the downsample rate
-- and the length of the original [pushData].
selectDownsampleMaxminAdaptive' :: [PushData] -> [PushData] -> Double -> IO [PushData]
selectDownsampleMaxminAdaptive' newDownsample casesToPickFrom cDelta = do
      let newDistances = map extractDistance newDownsample
      let minCaseDistances = minOfColumns (map (\distList -> filterByIndex distList (map extractIndex casesToPickFrom)) newDistances)
      selectedCaseIndex <- argMax minCaseDistances
      stdGen <- initStdGen
      if null casesToPickFrom || (maximum minCaseDistances <= cDelta)
      then pure newDownsample
      else selectDownsampleMaxminAdaptive'
            ((casesToPickFrom !! selectedCaseIndex) : newDownsample)
            (shuffle' (deleteAt selectedCaseIndex casesToPickFrom) (length casesToPickFrom - 1) stdGen)
            cDelta

-- |Returns the distance between two cases given a list of individual error vectors, and the index these
-- cases exist in the error vector. Only makes the distinction between zero and nonzero errors.
getDistanceBetweenCases :: [[Double]] -> Int -> Int -> Double
getDistanceBetweenCases errorLists caseIndex0 caseIndex1 =
  if lhe < caseIndex0 || lhe < caseIndex1 || caseIndex0 < 0 || caseIndex1 < 0
    then fromIntegral @Int @Double $ length errorLists
    else sum $ zipWith (\e0 e1 -> abs $ abs (signum e0) - abs (signum e1)) errors0 errors1
  where
    lhe :: Int -- length head errorLists
    lhe = length $ case uncons errorLists of Just (x, _) -> x; _ -> error "Error: errorLists is empty!"
    errors0 :: [Double]
    errors0 = map (\e -> case e !? caseIndex0 of Just x -> x; _ -> 0) errorLists
    errors1 :: [Double]
    errors1 = map (\e -> case e !? caseIndex1 of Just x -> x; _ -> 0) errorLists

-- |Updates a list with the values from another list based on an index from a third list.
-- The first list (bigList) has its indices updated with the values from the second list (smallList)
-- per index notated in the third [Int] list.
updateAtIndices :: [a] -> [a] -> [Int] -> [a]
updateAtIndices bigList _ [] = bigList
updateAtIndices bigList smallList indices =
  if length smallList /= length indices || any (\x -> x < 0 || x >= length bigList) indices
    then bigList
    else updateAtIndices' bigList smallList indices

-- |Look at updateAtIndicies for documentation. You should probably not
-- call this function. There is error checking in updateAtIndices, not this one.
updateAtIndices' :: [a] -> [a] -> [Int] -> [a]
updateAtIndices' bigList _ [] = bigList
updateAtIndices' bigList [] _ = bigList
updateAtIndices' bigList (sval:svals) (idx:idxs) = updateAtIndices' (replaceAt idx sval bigList) svals idxs 

-- |Merges two lists of [Pushdata], replacing the PushData in the lists with their corresponding
-- (based on index) PushData in the small list.
mergePushDataListsAtIndex :: [PushData] -> [PushData] -> [PushData]
mergePushDataListsAtIndex bigList smallList = map (\x -> let correspondingSmall = find (\y -> extractIndex x == extractIndex y) smallList in fromMaybe x correspondingSmall) bigList

-- |Replaces all ints of a list that equal the minimum int in said same list with 0.
replaceMinsWithZero :: (Num a, Ord a) => [a] -> [a]
replaceMinsWithZero xs = map (\x -> if minimum xs == x then 0 else x) xs

-- |Replaces values within a delta of zero with zero, mainly used for regression problems.
replaceCloseZeroWithZero :: (Num a, Ord a) => a -> [a] -> [a]
replaceCloseZeroWithZero delta = map (\x -> if delta >= x then 0 else x)

-- |Converts a set of errors into a list where all the elite errors are replaced with 0s so that we can use
-- it in the selection of downsamples with elite/not-elite selection.
convertToEliteError :: forall a. (Num a, Ord a) => [[a]] -> [[a]]
convertToEliteError = map (replaceMinsWithZero @a) -- crazy lambda reduction. Is it worth it here?

-- |Converts a set of errors into a list where all of the errors are replaced with within a delta.
convertToSoftError :: forall a. (Num a, Ord a) => a -> [[a]] -> [[a]]
convertToSoftError delta = map (delta `replaceCloseZeroWithZero`)

-- |Updates the cases distances when downsampling.
updateCaseDistances :: [Individual] -> [PushData] -> [PushData] -> String -> Double -> [PushData]
updateCaseDistances evaledPop downsampleData trainData informedDownsamplingType solutionThreshold =
  mergePushDataListsAtIndex trainData
    (mapIndexed (\idx dCase -> dCase{_caseDistances = Just (updateAtIndices (extractDistance dCase)
      (map (getDistanceBetweenCases corrErrors idx) [0..(length dsIndicies - 1)]) dsIndicies)}) downsampleData)
  where
    dsIndicies :: [Int]
    dsIndicies = map extractIndex downsampleData
    errors :: [[Double]]
    errors = map extractFitnessCases evaledPop
    corrErrors :: [[Double]]
    corrErrors = case informedDownsamplingType of
      "elite" -> convertToEliteError errors
      "soft" -> convertToSoftError solutionThreshold errors
      _ -> errors


-- map (\other -> getDistanceBetweenCases [[0,0],[0,0]] 0 other) [0..(length [3,4] - 1)]
-- tempData = intTrainData !! 0
-- dCase = tempData{_downsampleIndex = Just 3, _caseDistances = Just [2,2,2,2,2]}
-- updateIn dCase (updateAtIndices [2,2,2,2,2] (map (\other -> getDistanceBetweenCases [[0,0],[0,0]] 0 0) [0..(length [3,4] - 1)]) [3,4])
-- Replacement for updateIn: dCase{_caseDistances = Just (updateAtIndices (extractDistance dCase) (map (\other -> getDistanceBetweenCases [[0,0],[0,0]] 0 0) [0..(length [3,4] - 1)]) [3,4])}
