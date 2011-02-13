{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import System.Posix.Files
import Data.List
import Graphics.Vty
import Graphics.Vty.Widgets.All

isEmacsBackup :: FilePath -> FileStatus -> Bool
isEmacsBackup fp _ = ("~" `isSuffixOf` fp)

customAttrs :: [(FilePath -> FileStatus -> Bool, Attr)]
customAttrs = [ (isEmacsBackup, yellow `on` blue)
              ]

main :: IO ()
main = do
  b <- newDirBrowser $ defaultBrowserSkin `withCustomAttrs` customAttrs
  let ui = dirBrowserWidget b

  b `onBrowseAccept` (error . ("You chose: " ++))
  b `onBrowseCancel` const (error "Cancelled.")

  runUi ui $ defaultContext { focusAttr = (black `on` yellow)
                            }