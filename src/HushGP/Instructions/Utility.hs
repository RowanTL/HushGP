module HushGP.Instructions.Utility where

import Control.Lens hiding (index)
import HushGP.State
import Data.Char

-- generic utility

-- |Utility Function: Deletes an item from a list at a specified index.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs <> drop 1 (drop idx xs)

-- |Utility Function: Combines two tuples containing lists with a value placed between them.
combineTuple :: a -> ([a], [a]) -> [a]
combineTuple val = combineTupleList [val]

-- |Utility Function: Combines two tuples containing lists with a list placed between them.
combineTupleList :: [a] -> ([a], [a]) -> [a]
combineTupleList val tup = fst tup <> val <> snd tup

-- |Utility Function: Inserts a value based on an int at a specified index.
insertAt :: Int -> a -> [a] -> [a]
insertAt idx val xs = combineTuple val (splitAt idx xs)

-- |Utility Function: Replaces a value based on an int at a specified index.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = deleteAt (idx + 1) (insertAt idx val xs)

-- |Utility Function: Takes two ints as indices. Sorts them low to high, sets the start to
-- 0 if the lowest start is less than 0 and the end to the length of the list - 1 if the end
-- if larger than the list. Grabs the sub list of adjusted indices.
subList :: Int -> Int -> [a] -> [a]
subList idx0 idx1 xs =
  let
    (start, end) = if idx0 <= idx1 then (idx0, idx1) else (idx1, idx0)
    adjStart = max 0 start
    adjEnd = min end (length xs)
  in
    take adjEnd (drop adjStart xs)

-- |Utility Function: Finds the index of the second list inside of the first index.
-- If the sublist passed is larger than the full list, returns -1
-- If the lists are of equal length, and then contents are equal, returns 0. If not equal, returns -1
-- Recursively shortens the full list until the sub list is found.
findSubA :: forall a. Eq a => [a] -> [a] -> Int
findSubA fullA subA 
  | length fullA < length subA = -1
  | length fullA == length subA = if fullA == subA then 0 else -1
  | otherwise = findSubA' fullA subA 0
  where
    findSubA' :: [a] -> [a] -> Int -> Int
    findSubA' fA sA subIndex
      | null fA = -1
      | length sA > length fA = -1
      | sA == take (length sA) fA = subIndex
      | otherwise = findSubA' (drop 1 fA) sA (subIndex + 1)

-- |Utility Function: Replaces a number of instances of old with new in a list.
-- The Maybe Int is the amount of olds to replace with new. Nothing means replace all.
-- Just chain findSubA calls.
-- May not be the most efficient method with the findSubA calls.
replace :: Eq a => [a] -> [a] -> [a] -> Maybe Int -> [a]
replace fullA old new (Just amt) =
  if findSubA fullA old /= -1 && amt > 0
    then replace (take (findSubA fullA old) fullA <> new <> drop (findSubA fullA old + length old) fullA) old new (Just $ amt - 1)
    else fullA
replace fullA old new Nothing =
  if findSubA fullA old /= -1
    then replace (take (findSubA fullA old) fullA <> new <> drop (findSubA fullA old + length old) fullA) old new Nothing
    else fullA

-- |Utility Function: Counts the amount of occurrences of a sub list inside
-- of a larger list.
amtOccurences :: forall a. Eq a => [a] -> [a] -> Int
amtOccurences fullA subA = amtOccurences' fullA subA 0
  where
    amtOccurences' :: [a] -> [a] -> Int -> Int
    amtOccurences' fA sA count =
      if findSubA fA sA /= -1
        then amtOccurences' (replace fA sA mempty (Just 1)) sA (count + 1)
        else count

-- |Utility Function: Takes the last N elements of a list.
takeR :: Int -> [a] -> [a]
takeR amt fullA = drop (length fullA - amt) fullA

-- |Utility Function: Drops the last N elements of a list.
dropR :: Int -> [a] -> [a]
dropR amt fullA = take (length fullA - amt) fullA

-- |Utility Function: A safe version of init. If the list is empty, returns the empty list.
-- If the list has items, takes the init of the list.
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

-- |Utility Function: An indexing strategy used in parts of Hush. Takes the absolute value
-- of the passed number `mod` the length of the passed list.
absNum :: Integral a => a -> [b] -> Int
absNum rawNum lst = abs (fromIntegral rawNum) `mod` length lst

-- |Utility Function: Checks to see if a list is empty.
-- If the list is empty, returns False.
-- If the list is not empty, returns True.
notEmptyStack :: Lens' State [a] -> State -> Bool
notEmptyStack accessor state = not . null $ view accessor state

-- |Utility Function: Extracts an int from a GeneInt.
-- How to make this polymorphic???????? A general function for
-- this would be nice. Wrapped in a maybe too?
extractGeneInt :: Gene -> Integer
extractGeneInt (GeneInt x) = x
extractGeneInt _ = error "todo this later??"


-- bool utility

-- |A template function to make bool comparisons concise.
boolTemplate :: (Bool -> Bool -> Bool) -> State -> State
boolTemplate func state@(State {_bool = b1 : b2 : bs}) = state {_bool = func b1 b2 : bs}
boolTemplate _ state = state

-- |Utility function. Haskell doesn't have its own xor operation.
xor :: Bool -> Bool -> Bool
xor b1 b2
  | b1 && not b2 = True
  | not b1 && b2 = True
  | otherwise = False

-- char utility

-- |Utility: Converts a whole number `mod` 128 to a char.
intToAscii :: Integral a => a -> Char
intToAscii val = chr (abs (fromIntegral val) `mod` 128)

-- code utility

-- |Utility function: Checks to see if a gene is a code block.
-- If it is a block, returns true, else returns false
isBlock :: Gene -> Bool
isBlock (Block _) = True
isBlock _ = False

-- |Utility function: Returns the length of the passed block.
-- If the gene isn't a block, returns 1
blockLength :: Gene -> Integer
blockLength (Block bxs) = toInteger $ length bxs
blockLength _ = 1

-- |Utility function: Returns true if the passed block is empty, false is not.
-- If the passed gene is not a block, returns false
blockIsNull :: Gene -> Bool
blockIsNull (Block bxs) = null bxs
blockIsNull _ = False

-- |Utility Function: A helper function for instructionCodeContainer. The full description is there.
-- https://faculty.hampshire.edu/lspector/push3-description.html#Type
-- CODE.CONTAINER
findContainer :: Gene -> Gene -> Gene
findContainer (Block fullA) gene
  | fromIntegral (length fullA) <= blockLength gene = Block []
  | gene `elem` fullA = Block [] -- Not allowed to be top level
  | any isBlock fullA = findContainer' (filter isBlock fullA) gene
  | otherwise = Block []
  where
    findContainer' :: [Gene] -> Gene -> Gene
    findContainer' [] _ = Block []
    findContainer' ((Block bx1) : bxs) g = if g `elem` bx1 then Block bx1 else findContainer' bxs g
    findContainer' _ _ = Block [] -- This should never happen
findContainer _ _ = Block []

-- |Utility Function: A helper function for instructionCodeDiscrepancy. The full description is there.
countDiscrepancy :: Gene -> Gene -> Integer
-- countDiscrepancy (Block xs) (Block ys) = sum [if uncurry (==) tup then 0 else 1 | tup <- zip xs ys] + abs (toInteger (length xs) - toInteger (length ys))
-- countDiscrepancy (Block xs) (Block ys) = sum [if isBlock (fst tup) && isBlock (snd tup) then uncurry countDiscrepancy tup else if uncurry (==) tup then 0 else 1 | tup <- zip xs ys] + abs (toInteger (length xs) - toInteger (length ys))
countDiscrepancy (Block xs) (Block []) = codeRecursiveSize (Block xs)
countDiscrepancy (Block []) (Block ys) = codeRecursiveSize (Block ys)
countDiscrepancy (Block (x:xs)) (Block (y:ys)) = if x == y then 1 + countDiscrepancy (Block xs) (Block ys) else countDiscrepancy (Block xs) (Block ys)
countDiscrepancy _ (Block ys) = 1 + codeRecursiveSize (Block ys)
countDiscrepancy (Block xs) _ = 1 + codeRecursiveSize (Block xs)
countDiscrepancy xgene ygene = if xgene == ygene then 1 else 0

-- |Utility Function: Extracts the first gene from a block. Returns itself if not a block
extractFirstFromBlock :: Gene -> Gene
extractFirstFromBlock (Block (bx1 : _)) = bx1
extractFirstFromBlock gene = gene

-- |Utility Function: Returns the last gene from a block, [] if the block is empty, and itself if not a block
extractLastFromBlock :: Gene -> Gene
extractLastFromBlock (Block []) = Block []
extractLastFromBlock (Block bxs) = last bxs
extractLastFromBlock gene = gene

-- |Utility Function: Calls init on a block. If the block is empty, returns []. If gene isn't a block, returns itself
extractInitFromBlock :: Gene -> Gene
extractInitFromBlock (Block bxs) = Block (safeInit bxs)
extractInitFromBlock gene = gene

-- |Utility Function: Calls `drop 1` on a block. If gene isn't a block, returns itself
extractTailFromBlock :: Gene -> Gene
extractTailFromBlock (Block bxs) = Block (drop 1 bxs)
extractTailFromBlock _ = Block []

-- |Utility Function: Extracts the code at a point in the genome. Recurses into a nested Block if found. The
-- point is based on an int.
codeAtPoint :: [Gene] -> Int -> Gene
codeAtPoint (gene : _) 0 = gene
codeAtPoint [] _ = Block [] -- Should only happen if an empty block is last Gene in the list of Genes
codeAtPoint ((Block nestedGenes) : genes) index = codeAtPoint (nestedGenes <> genes) (index - 1)
codeAtPoint (_ : genes) index = codeAtPoint genes (index - 1)

-- |Utility Function: Inserts code at a point in the genome. Recurses into a block if found. The point is based
-- on an integer
codeInsertAtPoint :: [Gene] -> Gene -> Int -> [Gene]
codeInsertAtPoint oldGenes gene 0 = gene : oldGenes
codeInsertAtPoint [] gene _ = [gene] -- This shouldn't happen (lol)
codeInsertAtPoint ((Block genes) : oldGenes) gene index = Block (codeInsertAtPoint genes gene (index - 1)) : oldGenes
codeInsertAtPoint (oldGene : oldGenes) gene index = oldGene : codeInsertAtPoint oldGenes gene (index - 1) 

-- |Utility Function: Combines two genes together into a block.
codeCombine :: Gene -> Gene -> Gene
codeCombine (Block bxs) (Block bys) = Block (bxs <> bys)
codeCombine (Block bxs) ygene = Block (ygene : bxs)
codeCombine xgene (Block bys) = Block (xgene : bys)
codeCombine xgene ygene = Block [xgene, ygene]

-- |Utility Function: Determines if the second gene is a member of the first gene.
-- If the first gene is a Block and the second gene is also a Block, does a sublist search for the second block in the first block.
-- if the first gene is a Block and the second gene is not, the block is searched for the second gene.
-- If neither of the genes are blocks, returns False.
codeMember :: Gene -> Gene -> Bool
codeMember (Block bxs) (Block bys) = findSubA bxs bys /= (-1)
codeMember (Block bxs) ygene = ygene `elem` bxs
codeMember _ _ = False

-- |Utility Function: Calculates the size of a Block including counting the nested Blocks recursively
codeRecursiveSize :: Gene -> Integer
codeRecursiveSize (Block bxs) = sum [codeRecursiveSize x + if isBlock x then 1 else 0 | x <- bxs]
codeRecursiveSize _ = 1

-- string utility

-- |Utility String: Whitespack characters.
-- shamelessly stolen from https://hackage.haskell.org/package/MissingH-1.6.0.1/docs/src/Data.String.Utils.html#strip
wschars :: String
wschars = " \t\r\n"

-- |Utility Function: Strips a string of its whitespace on both sides.
strip :: String -> String
strip = lstrip . rstrip

-- |Utility Function: Strips a string of its whitespace on the left side.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if x `elem` wschars
                            then lstrip xs
                            else s

-- |Utility Function: Strips a string of its whitespace on the right side.
-- this is a tad inefficient
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- string utility

-- |Utility Function: Casts a type based on a lens to a string. Pushes the result
-- to the string stack.
instructionStringFromLens :: Show a => Lens' State [a] -> State -> State
instructionStringFromLens accessor state@(State {_string = ss}) =
  case uncons (view accessor state) of
    Nothing -> state
    Just (x1,_) -> state{_string = show x1 : ss}
