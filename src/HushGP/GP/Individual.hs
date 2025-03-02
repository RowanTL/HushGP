module HushGP.GP.Individual where

import HushGP.State

-- | The structure for an individual containing the genome, the totalFitness, and
-- the individual fitness cases for lexicase.
data Individual = Individual
  { plushy :: [Gene],
    totalFitness :: Maybe Double,
    fitnessCases :: Maybe [Double],
    selectionCases :: Maybe [Int]
  }
  deriving (Show, Eq)

instance Ord Individual where
  ind0 <= ind1 = totalFitness ind0 <= totalFitness ind1

-- | Extracts the fitnessCases from an Individual. Errors if the field is empty.
extractFitnessCases :: Individual -> [Double]
extractFitnessCases Individual {fitnessCases = Nothing} = error "Error: fitnessCases is empty!"
extractFitnessCases Individual {fitnessCases = Just xs} = xs

extractTotalFitness :: Individual -> Double
extractTotalFitness Individual {totalFitness = Nothing} = error "Error: totalFitness is empty!"
extractTotalFitness Individual {totalFitness = Just x} = x
