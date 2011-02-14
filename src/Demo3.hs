module Main where

import System.Posix.Files
import Data.List
import Graphics.Vty
import Graphics.Vty.Widgets.All

customAnnotations :: [( FilePath -> FileStatus -> Bool
                      , FilePath -> FileStatus -> IO String
                      , Attr)]
customAnnotations = [ (\p _ -> "~" `isSuffixOf` p
                      , \_ _ -> return "emacs backup file"
                      , yellow `on` blue)
                    ]

main :: IO ()
main = do
  (b, fg) <- newDirBrowser $ defaultBrowserSkin
             `withAnnotations` customAnnotations

  b `onBrowseAccept` \p ->
      if "~" `isSuffixOf` p
      then error $ "You chose: " ++ p
      else reportBrowserError b "Please select an emacs backup file."

  b `onBrowseCancel` const (error "Cancelled.")

  c <- newCollection
  _ <- addToCollection c (dirBrowserWidget b) fg

  runUi c $ defaultContext { focusAttr = (black `on` yellow)
                           }