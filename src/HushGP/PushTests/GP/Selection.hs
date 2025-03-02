module HushGP.PushTests.GP.Selection where

import Data.List
import HushGP.GP.Individual
import HushGP.State
import HushGP.Utility

-- | One of the steps in the lexicase selection process for selecting initial survivors.
tempFunc0 :: [[Individual]]
tempFunc0 = groupBy (\x y -> fitnessCases x == fitnessCases y) testInds

-- \| Another step forward in the lexicase selection process.
survivors :: IO [Individual]
survivors = mapM randElem tempFunc0

-- | A list of individuals used for testing.
testInds :: [Individual]
testInds =
  [ Individual{plushy = [Close], totalFitness = Just 1000, fitnessCases = Just [500,500]}
  , Individual{plushy = [Close], totalFitness = Just 1000, fitnessCases = Just [400,600]}
  , Individual{plushy = [Close], totalFitness = Just 900, fitnessCases = Just [500,400]}
  ]
