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

customAnnotations :: [(FilePath -> FileStatus -> Bool, FilePath -> FileStatus -> IO String)]
customAnnotations = [ (\p _ -> "~" `isSuffixOf` p, \_ _ -> return "emacs backup file")
                    ]

main :: IO ()
main = do
  b <- newDirBrowser $ defaultBrowserSkin `withCustomAttrs` customAttrs
       `withCustomAnnotations` customAnnotations

  let ui = dirBrowserWidget b

  b `onBrowseAccept` (error . ("You chose: " ++))
  b `onBrowseCancel` const (error "Cancelled.")

  runUi ui $ defaultContext { focusAttr = (black `on` yellow)
                            }