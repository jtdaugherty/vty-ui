{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  b <- newDirBrowser "/" defaultBrowserSkin
  let ui = dirBrowserWidget b

  fg <- newFocusGroup
  addToFocusGroup fg (dirBrowserList b)
  setFocusGroup ui fg

  (dirBrowserList b) `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        _ -> return False

  runUi ui $ defaultContext { focusAttr = (black `on` yellow)
                            }