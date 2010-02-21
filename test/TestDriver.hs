{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import System.Exit ( exitFailure, exitSuccess )
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Tests.Text as Text

tests :: [Property]
tests = Text.tests

main :: IO ()
main = do
  results <- mapM quickCheckResult tests
  if all isSuccess results then
      exitSuccess else
      exitFailure
