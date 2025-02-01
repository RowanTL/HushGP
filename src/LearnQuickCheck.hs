module LearnQuickCheck where

-- https://jesper.sikanda.be/posts/quickcheck-intro.html

import Test.QuickCheck
import Data.List (sort)
import Control.Monad

qsort :: Ord a => [a] -> [a]
qsort = sort

distance :: Int -> Int -> Int
distance x y = abs (x - y)

prop_dist35 :: Bool
prop_dist35 = distance 3 5 == 2

prop_dist_self :: Int -> Bool
prop_dist_self x = distance x x == 0

prop_dist_symmetric :: Int -> Int -> Bool
prop_dist_symmetric x y = distance x y == distance y x

bad_distance :: Int -> Int -> Int
bad_distance x y = y - x

prop_dist_symmetric_fail :: Int -> Int -> Bool
prop_dist_symmetric_fail x y = bad_distance x y == bad_distance y x

sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _ = True

prop_sorted :: [Int] -> Bool
prop_sorted xs = sorted xs

-- roundtrip property
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y:insert x ys

delete :: Int -> [Int] -> [Int]
delete x [] = []
delete x (y:ys) | x == y    = ys
                | otherwise = y:delete x ys

prop_insert_delete :: [Int] -> Int -> Bool
prop_insert_delete xs x = delete x (insert x xs) == xs

-- Equivalent Property
prop_qsort_sort :: [Int] -> Bool
prop_qsort_sort xs = qsort xs == sort xs

-- can test this in ghci with verboseCheck
prop_qsort_sort' :: Ord a => [a] -> Bool
prop_qsort_sort' xs = qsort xs == sort xs

-- Algebraic Laws
vAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)  
vAdd tup0 tup1 = (fst tup0 + fst tup1, snd tup0 + snd tup1)

prop_vAdd_commutative :: (Int,Int) -> (Int,Int) -> Bool
prop_vAdd_commutative v w = vAdd v w == vAdd w v

prop_vAdd_associative :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
prop_vAdd_associative u v w = vAdd (vAdd u v) w == vAdd u (vAdd v w)

prop_vAdd_neutral_left :: (Int,Int) -> Bool
prop_vAdd_neutral_left u = vAdd (0,0) u == u

prop_vAdd_neutral_right :: (Int,Int) -> Bool
prop_vAdd_neutral_right u = vAdd u (0,0) == u

prop_qsort_idempotent :: [Int] -> Bool
prop_qsort_idempotent xs = qsort (qsort xs) == qsort xs

-- Testing with different distributions section
-- prop_replicate :: Int -> Int -> Int -> Bool
-- prop_replicate n x i = replicate n x !! i == x

prop_replicate :: Int -> Int -> Int -> Property
prop_replicate n x i = 
  (i >= 0 && i < n) ==> replicate n (x :: Int) !! i == x

prop_insert_sorted :: Int -> [Int] -> Property
prop_insert_sorted x xs = sorted xs ==> sorted (insert x xs)

-- Quantified properties
prop_insert_sorted' :: Int -> Property
prop_insert_sorted' x = forAll orderedList (\xs -> sorted (insert x xs))

-- Testing properties of functions
prop_filter :: Fun Int Bool -> [Int] -> Property
prop_filter p xs = 
      -- Filter elements not satisfying p.
  let ys = [ x | x <- xs , applyFun p x ]
      -- If any elements are left...  
  in  ys /= [] ==>          
        -- ...generate a random index i...               
        forAll (choose (0,length ys-1))
          -- ...and test if p (ys!!i) holds.    
          (\i -> applyFun p (ys!!i))



prop_bananas :: Fun String Int -> Bool
prop_bananas f = 
  applyFun f "banana" == applyFun f "monkey" ||
  applyFun f "banana" == applyFun f "elephant" ||
  applyFun f "monkey" == applyFun f "elephant"

-- main :: IO ()
-- main = do
--   quickCheck prop_dist35
--   quickCheck prop_dist_self
--   quickCheck prop_dist_symmetric
--   -- Roundtrip tests
--   quickCheck prop_insert_delete
--   -- Equivalent tests
--   quickCheck prop_qsort_sort
--   -- quickCheck prop_qsort_sort'
--   -- Algebraic tests
--   quickCheck prop_vAdd_commutative
--   quickCheck prop_vAdd_associative
--   quickCheck prop_vAdd_neutral_left
--   quickCheck prop_vAdd_neutral_right
--   -- Testing with different distributions
--   quickCheck prop_replicate
--   quickCheck prop_insert_sorted
--   -- Quantified Properties
--   quickCheck prop_insert_sorted'
--   -- Testing properties of functions
--   quickCheck prop_filter
--   quickCheck prop_bananas

-- This next section is from the Practical Property Testing video on youtube
-- by FP Complete Corporation

genSuit, genVal :: Gen Char
genSuit = elements "HDCS"
genVal = elements "123456789JQK"

-- Applicative so can do this
genCard :: Gen (Char, Char)
genCard = (,) <$> genSuit <*> genVal

-- Monad so can do this
genCards :: Gen [(Char, Char)]
genCards = do
  l <- arbitrary
  replicateM l genCard

genListOf15Ints :: Gen [Int]
genListOf15Ints = resize 15 $ sized $ \n -> replicateM n arbitrary
