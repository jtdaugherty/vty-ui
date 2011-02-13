{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  b <- newDirBrowser defaultBrowserSkin
  let ui = dirBrowserWidget b

  b `onBrowseAccept` (error . ("You chose: " ++))
  b `onBrowseCancel` const (error "Cancelled.")

  runUi ui $ defaultContext { focusAttr = (black `on` yellow)
                            }