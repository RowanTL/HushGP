module HushGP.TH where

import Data.List
import Language.Haskell.TH
import Text.Regex.TDFA

-- This old code made this all possible :)
-- https://github.com/finnsson/template-helper/blob/master/src/Language/Haskell/Extract.hs

-- | A way to extract all functions from the file
--  `lines file` pulls all of the lines in one string from the file
--  `lex $ lines file` splits the function into a tuple
--    fst = the function nams, snd = the rest of the line
--  `concatMap lex $ lines file` maps lex onto all of the lines
--    and concats the result into a list
--  `filter (=~pattern) $ map fst $ concatMap lex $ lines file` filters
--    any line that doesn't have the passed pattern to it. "function" is a good pattern
--    for Hush.
--  `nub $ filter (=~pattern) $ map fst $ concatMap lex $ lines file` removes all
--    duplicates from the list. Or sets in this case :)
extractAllFunctions :: String -> Q [String]
extractAllFunctions pattern = do
  loc <- location
  -- file <- runIO $ readFile pattern
  file <- runIO $ readFile $ loc_filename loc
  pure $ nub $ filter (=~ pattern) $ map fst $ concatMap lex $ lines file

-- | Extracts all functions from a Q [String] (to be used with extractAllFunctions)
--  funcs has a list of all functions from extractAllFunctions
--  makePair makes a tuple of a passed function holding its name as a string and actual function value
--    in that order. StateFunc :)
--  `ListE $ map makePair funcs` makes a list of these function tuples holding all function
--    names and values.
functionExtractor :: String -> Q Exp
functionExtractor pattern = do
  funcs <- extractAllFunctions pattern
  let makePair n = TupE [Just $ VarE $ mkName n, Just $ LitE $ StringL n]
  pure $ ListE $ map makePair funcs
