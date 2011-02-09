{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import System.Exit
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  b1 <- (simpleText "One\nTwo\nThree") <++> (simpleText "Cake\nCookies\nDonuts")
  b2 <- (simpleText "Foo\nBar\nBaz") <--> (return b1)

  setBoxSpacing b1 1
  setBoxSpacing b2 1

  ui <- bordered b2

  setBoxChildSizePolicy b1 $ Percentage 30
  setBoxChildSizePolicy b2 (PerChild (BoxFixed 10) BoxAuto)

  fg <- newFocusGroup
  addToFocusGroup fg ui
  setFocusGroup ui fg

  ui `onKeyPressed` \_ _ _ -> exitSuccess

  runUi ui defaultContext