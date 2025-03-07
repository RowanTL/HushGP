module HushGP.GP.Simplification where

import System.Random.Shuffle
import System.Random
import Control.Monad
import Data.List
import HushGP.State
import HushGP.GP.PushArgs

-- | Takes a list of Genes (a plushy), chunks it up into sizes of 1 (type is [[Gene]]).
-- and a list of indices for replacement (gets sorted before replacement).
deleteAtMultiple :: [Int] -> [Gene] -> [Gene]
deleteAtMultiple idxs = deleteAtMultiple' 0 (sort idxs)

-- | Internals of replaceAtMultiple. Takes a chunked plushy and replaces indices
-- that match the current index as specified at the idx.
deleteAtMultiple' :: Int -> [Int] -> [Gene] -> [Gene]
deleteAtMultiple' _ [] plushy = plushy
deleteAtMultiple' _ _ [] = []
deleteAtMultiple' curr (idx:idxs) (plushyPiece:plushy) =
  if curr == idx then deleteAtMultiple' (curr + 1) idxs plushy else plushyPiece : deleteAtMultiple' (curr + 1) (idx:idxs) plushy

-- | Deletes a random amount of genes from the passed plushy based on ant int.
deleteRandomAmt :: Int -> [Gene] -> IO [Gene]
deleteRandomAmt k plushy = do
  randomIndicies <- take k . shuffle' [0..(length plushy - 1)] (length plushy) <$> initStdGen
  pure $ deleteAtMultiple randomIndicies plushy

-- | Simplifies a Plushy by randomly deleting instructions and seeing how it impacts
-- performance. Removes genes that have zero to negative performance impact.
autoSimplifyPlushy :: PushArgs -> [Gene] -> IO [Gene]
autoSimplifyPlushy pushArgs@PushArgs{simplificationVerbose = simpVerbose, errorFunction = eFunc, trainingData = tData} plushy = do
  when simpVerbose (print ("simplification start plushy length: " <> show (length plushy)))
  autoSimplifyPlushy' pushArgs (eFunc pushArgs tData plushy) 0 plushy

-- | Internals for autosimplification. Keeps track of the amount of steps.
autoSimplifyPlushy' :: PushArgs -> [Double] -> Int -> [Gene] -> IO [Gene]
autoSimplifyPlushy' pushArgs@PushArgs{simplificationVerbose = simpVerbose, simplificationSteps = simpSteps, simplificationMaxAmt = simpK, errorFunction = eFunc, trainingData = tData} initialErrors step plushy
  | step < simpSteps = do
      randAmt <- fst . uniformR (1 :: Int, simpK) <$> initStdGen
      newPlushy <- deleteRandomAmt randAmt plushy
      let newPlushyErrors = eFunc pushArgs tData newPlushy
      let isBetter = newPlushyErrors <= initialErrors
      autoSimplifyPlushy' pushArgs initialErrors (succ step) (if isBetter then newPlushy else plushy)
  | otherwise = do
      when simpVerbose (print ("simplification end plushy length: " <> show (length plushy)))
      pure plushy
