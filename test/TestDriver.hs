{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import System.Exit ( exitFailure, exitSuccess )
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Tests.Text as Text
import qualified Tests.Tokenize as Tokenize

tests :: [Property]
tests = concat [ Text.tests
               , Tokenize.tests
               ]

main :: IO ()
main = do
  results <- mapM (quickCheckWithResult (stdArgs { maxSuccess = 200 })) tests
  if all isSuccess results then
      exitSuccess else
      exitFailure
