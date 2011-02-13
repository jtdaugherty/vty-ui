{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Maybe
import System.Exit
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  e <- editWidget
  fg <- newFocusGroup
  addToFocusGroup fg e

  u <- simpleText "Enter some text and press enter." <--> return e
       >>= withBoxSpacing 1

  pe <- padded u (padLeftRight 2)
  d <- newDialog pe "<enter text>" (Just fg) >>= withNormalAttribute (white `on` blue)

  c <- centered =<< withPadding (padLeftRight 10) (dialogWidget d)
  (setFocusGroup c . fromJust) =<< (getFocusGroup $ dialogWidget d)

  -- When the edit widget changes, set the dialog's title.
  e `onChange` setDialogTitle d

  -- When the user presses Enter in the edit widget, accept the
  -- dialog.
  e `onActivate` (const $ acceptDialog d)

  -- Exit either way.
  d `onDialogAccept` const exitSuccess
  d `onDialogCancel` const exitSuccess

  runUi c $ defaultContext { focusAttr = black `on` yellow }
