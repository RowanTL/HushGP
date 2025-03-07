module HushGP.Genome where

import Data.List
import Data.List.Split
import Data.Map qualified as Map
import HushGP.GP.Individual
import HushGP.GP.PushArgs
import HushGP.Instructions.Opens
import HushGP.State
import HushGP.Utility

-- | Makes a random individual based on the variables in a passed PushArgs.
makeRandomIndividual :: PushArgs -> IO Individual
makeRandomIndividual pushArgs = do
  randomPlushy <- makeRandomPlushy pushArgs
  return Individual {plushy = randomPlushy, totalFitness = Nothing, fitnessCases = Nothing, selectionCases = Nothing}

-- | Makes a random plushy from variables in a passed PushArgs.
makeRandomPlushy :: PushArgs -> IO [Gene]
makeRandomPlushy PushArgs {maxInitialPlushySize = maxInitPSize, instructionList = iList} = randomInstructions maxInitPSize iList

-- | A utility function to generate an amount based on an int rather than
--  from an argmap.
makeRandomPlushy' :: Int -> [Gene] -> IO [Gene]
makeRandomPlushy' = randomInstructions

-- | Checks to see if a Gene is an (Open _) constructor.
isOpen :: Gene -> Bool
isOpen (Open _) = True
isOpen _ = False

-- | Decrements the count of an (Open _) constructor. Acts as id
--  if the gene isn't an open.
decOpen :: Gene -> Gene
decOpen (Open n) = Open (n - 1)
decOpen gene = gene

-- | Checks to see if the a list of genes with a single element is an opener.
isOpenerList :: [Gene] -> Bool
isOpenerList [instruction] =
  case Map.lookup instruction instructionOpens of
    Just _ -> True
    _ -> False
isOpenerList _ = False

-- | Checks if the Gene is a Gap, returns True if it is.
isGap :: Gene -> Bool
isGap Gap = True
isGap _ = False

-- | Gets the amount of blocks to open from a list of genes with a single element.
getOpenAmountList :: [Gene] -> Int
getOpenAmountList [instruction] =
  case Map.lookup instruction instructionOpens of
    Just amt -> amt
    _ -> 0
getOpenAmountList _ = 0

-- | Converts a plushy genome into a push genome.
plushyToPush :: PushArgs -> [Gene] -> [Gene]
plushyToPush PushArgs {useBMX = bmx} plushy = plushyToPush' modPlushy []
  where
    modPlushy :: [Gene]
    modPlushy =
      if bmx
        then concatMap (filter (not . isGap) . (\x -> if isOpenerList x then x <> [Open (getOpenAmountList x)] else x)) (chunksOf 1 plushy)
        else concatMap (\x -> if isOpenerList x then x <> [Open (getOpenAmountList x)] else x) (chunksOf 1 plushy)

-- | Internal function used to convert a plushy genome with opens in it into its push phenotype.
plushyToPush' :: [Gene] -> [Gene] -> [Gene]
plushyToPush' openPlushy push
  | null openPlushy =
      if any isOpen push
        then plushyToPush' [Close] push
        else push
  | firstPlushy == Close =
      if any isOpen push
        then plushyToPush' (drop 1 openPlushy) (if numOpen (push !! openIndex) == 1 then preOpen <> [Block postOpen] else preOpen <> [Block postOpen] <> [decOpen (Open (numOpen (push !! openIndex)))])
        else plushyToPush' (drop 1 openPlushy) push
  | firstPlushy == Skip =
      case uncons openPlushy of
        Just (_, _ : xs) -> plushyToPush' xs push
        _ -> plushyToPush' (drop 1 openPlushy) push
  | otherwise = plushyToPush' (drop 1 openPlushy) (push <> [firstPlushy])
  where
    firstPlushy :: Gene
    firstPlushy =
      case uncons openPlushy of
        Just (g, _) -> g
        _ -> error "Error: First plushy taken when no plushy available!"
    postOpen :: [Gene]
    postOpen = reverse (takeWhile (not . isOpen) (reverse push))
    openIndex :: Int
    openIndex = length push - length postOpen - 1
    numOpen :: Gene -> Int
    numOpen (Open n) = n
    numOpen _ = 0
    preOpen :: [Gene]
    preOpen = take openIndex push
