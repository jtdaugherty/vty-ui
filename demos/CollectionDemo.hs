{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

items :: [T.Text]
items = [ "Arrow keys change list items"
        , "Esc quits"
        , "Ctrl-n goes to the next interface"
        ]

mkFirstUI = do
  u <- plainText $ T.unlines items
  pe <- padded u (padLeftRight 2)
  (d, dFg) <- newDialog pe "This is a dialog"
  setNormalAttribute d (white `on` blue)
  c <- centered =<< withPadding (padLeftRight 10) (dialogWidget d)
  d `onDialogAccept` const shutdownUi
  d `onDialogCancel` const shutdownUi
  return (c, dFg)

mkSecondUI = do
  fg <- newFocusGroup
  lst <- newTextList (green `on` blue) items 1
  addToFocusGroup fg lst
  c <- centered =<< vLimit 10 =<< hLimit 50 =<< bordered lst
  return (c, fg)

mkThirdUI = do
  fg <- newFocusGroup
  cb1 <- newCheckbox $ items !! 0
  cb2 <- newCheckbox $ items !! 1
  cb3 <- newCheckbox $ items !! 2
  addToFocusGroup fg cb1
  addToFocusGroup fg cb2
  addToFocusGroup fg cb3

  c <- centered =<< vLimit 10 =<< hLimit 50 =<< bordered =<< (
         (return cb1) <--> (return cb2) <--> (return cb3)
       )

  return (c, fg)

main :: IO ()
main = do
  coll <- newCollection

  (ui1, fg1) <- mkFirstUI
  switchToFirst <- addToCollection coll ui1 fg1

  (ui2, fg2) <- mkSecondUI
  switchToSecond <- addToCollection coll ui2 fg2

  (ui3, fg3) <- mkThirdUI
  switchToThird <- addToCollection coll ui3 fg3

  fg1 `onKeyPressed` \_ k mods ->
      case (k, mods) of
          (KASCII 'n', [MCtrl]) -> switchToSecond >> return True
          (KEsc, []) -> exitSuccess
          _ -> return False

  fg2 `onKeyPressed` \_ k mods ->
      case (k, mods) of
          (KASCII 'n', [MCtrl]) -> switchToThird >> return True
          (KEsc, []) -> exitSuccess
          _ -> return False

  fg3 `onKeyPressed` \_ k mods ->
      case (k, mods) of
          (KASCII 'n', [MCtrl]) -> switchToFirst >> return True
          (KEsc, []) -> exitSuccess
          _ -> return False

  runUi coll $ defaultContext { focusAttr = black `on` yellow }
