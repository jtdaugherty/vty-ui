module Main where

import Graphics.Vty
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  (b, fg) <- newDirBrowser defaultBrowserSkin

  c <- newCollection
  addToCollection c (dirBrowserWidget b) fg

  b `onBrowseAccept` error
  b `onBrowseCancel` error

  runUi c $ defaultContext { focusAttr = white `on` blue
                           }
