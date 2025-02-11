{-# LANGUAGE TemplateHaskell #-}
module HushGP.TH where

import System.IO
import Text.Regex.TDFA
import Data.List
import HushGP.State
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- listFields :: Name -> Q [Dec]
-- listFields name = do
  -- TyConI (DataD _ _ _ [RecC _ fields] _ ) <- reify name

strHead :: [String] -> String
strHead strxs =
  case uncons strxs of
    Just (str, _) -> str
    _ -> []

instructionRegex :: String
instructionRegex = "instruction[a-zA-Z0-9]* ::" 

testRegex :: String -> Bool
testRegex str = str =~ instructionRegex :: Bool

thTest :: IO ()
thTest = do
  handle <- openFile "src/HushGP/Instructions/IntInstructions.hs" ReadMode
  let list = hGetContents handle
  toPrint <- list
  let funcs = map (strHead . words) (filter testRegex (lines toPrint))
  let names = map (newName :: (String -> IO Name)) funcs
  hClose handle
  print "hello"
  -- let instruction = "instructionIntAdd"
