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
  lst <- newTextList items 1
  setSelectedUnfocusedAttr lst $ Just (green `on` blue)
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

  let keyHandler nextUI = \_ k mods ->
        case (k, mods) of
            (KChar 'n', [MCtrl]) -> nextUI >> return True
            (KEsc, []) -> exitSuccess
            _ -> return False

  fg1 `onKeyPressed` (keyHandler switchToSecond)
  fg2 `onKeyPressed` (keyHandler switchToThird)
  fg3 `onKeyPressed` (keyHandler switchToFirst)

  runUi coll $ defaultContext { focusAttr = black `on` yellow }
