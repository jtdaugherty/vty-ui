module Tests.TextZipper
    ( tests
    )
where

import Control.Applicative
import Test.QuickCheck

import Tests.Instances ()
import qualified Data.Text as T
import Graphics.Vty.Widgets.TextZipper

textGen :: Gen T.Text
textGen = T.pack <$> (listOf $ oneof [ pure 'a'
                                     , pure ' '
                                     ])

docGen :: Gen [T.Text]
docGen = listOf1 textGen

validCursorPositions :: [T.Text] -> Gen (Int, Int)
validCursorPositions ts = do
  r <- elements [0..length ts - 1]
  c <- elements [0..T.length (ts !! r)]
  return (r, c)

cursorCheck :: Property
cursorCheck =
    forAll docGen $ \doc ->
        forAll (validCursorPositions doc) $ \pos ->
            pos == cursorPosition (moveCursor pos $ textZipper doc)

insertDelCheck :: Property
insertDelCheck =
    forAll docGen $ \doc ->
        forAll (validCursorPositions doc) $ \pos ->
            let orig = moveCursor pos $ textZipper doc
            in orig == (deletePrevChar $ insertChar 'X' orig)

tests :: [Property]
tests = [ label "get/set cursor position are consistent" cursorCheck
        , label "insert/delete are inverses" insertDelCheck
        ]