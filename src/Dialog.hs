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

  let updateTitle = setDialogTitle d =<< getEditText e

  e `onChange` \_ _ -> updateTitle
  e `onActivate` \_ -> acceptDialog d

  d `onDialogAccept` const exitSuccess
  d `onDialogCancel` const exitSuccess

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi c $ defaultContext { focusAttr = black `on` yellow }
