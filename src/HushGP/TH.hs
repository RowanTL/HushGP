{-# LANGUAGE TemplateHaskell #-}
module HushGP.TH where

import System.IO

thTest :: IO ()
thTest = do
  handle <- openFile "src/HushGP/Instructions/IntInstructions.hs" ReadMode
  let list = hGetContents handle
  toPrint <- list
  print toPrint
  hClose handle
