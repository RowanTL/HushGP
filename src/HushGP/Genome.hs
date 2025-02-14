module HushGP.Genome where

import Data.List
import Data.List.Split
import Data.Map qualified as Map
import HushGP.Instructions.Opens
import HushGP.State
import HushGP.Utility

-- | Makes a random plushy from variables in a passed argMap and
--  a passed list of instructions.
makeRandomPlushy :: Map.Map String String -> [Gene] -> IO [Gene]
makeRandomPlushy argMap = randomInstructions (read @Int (argMap Map.! "maxInitialPlushySize"))

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

-- | Gets the amount of blocks to open from a list of genes with a single element.
getOpenAmountList :: [Gene] -> Int
getOpenAmountList [instruction] =
  case Map.lookup instruction instructionOpens of
    Just amt -> amt
    _ -> 0
getOpenAmountList _ = 0

-- | Converts a plushy genome into a push genome.
plushyToPush :: [Gene] -> [Gene]
plushyToPush plushy = plushyToPush' (concatMap (\x -> if isOpenerList x then x <> [Open (getOpenAmountList x)] else x) (chunksOf 1 plushy)) []

-- | Internal function used to convert a plushy genome with opens in it into a push genome.
plushyToPush' :: [Gene] -> [Gene] -> [Gene]
plushyToPush' openPlushy push =
  if null openPlushy
    then
      if any isOpen push
        then plushyToPush' [Close] push
        else push
    else
      if firstPlushy == Close
        then
          if any isOpen push
            then plushyToPush' (drop 1 openPlushy) (if numOpen (push !! openIndex) == 1 then preOpen <> postOpen else preOpen <> postOpen <> [decOpen (Open (numOpen (push !! openIndex)))])
            else plushyToPush' (drop 1 openPlushy) push
        else plushyToPush' (drop 1 openPlushy) (push <> [firstPlushy])
  where
    firstPlushy :: Gene
    firstPlushy =
      case uncons openPlushy of
        Just (g, _) -> g
        _ -> error "This shouldn't happen"
    postOpen :: [Gene]
    postOpen = reverse (takeWhile (not . isOpen) (reverse push))
    openIndex :: Int
    openIndex = length push - length postOpen - 1
    numOpen :: Gene -> Int
    numOpen (Open n) = n
    numOpen _ = 0
    preOpen :: [Gene]
    preOpen = take openIndex push
