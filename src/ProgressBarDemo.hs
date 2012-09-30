{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}
module Main where

import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All

-- Visual attributes.
focAttr = black `on` green
bodyAttr = bright_green `on` black
completeAttr = white `on` red
incompleteAttr = red `on` white

setupProgessBar :: IO (Widget ProgressBar)
setupProgessBar = do
  pb <- newProgressBar completeAttr incompleteAttr
  setProgressTextAlignment pb AlignCenter

  pb `onProgressChange` \val ->
      setProgressText pb $ T.pack $ "Progress (" ++ show val ++ " %)"

  return pb

setupProgressBarThread :: Widget ProgressBar -> IO ()
setupProgressBarThread pb = do
  forkIO $ forever $ do
    let act i = do
          threadDelay $ 1 * 1000 * 1000
          schedule $ setProgress pb (i `mod` 101)
          act $ i + 4
    act 0
  return ()

setupDialog :: (Show a) => Widget a -> IO (Dialog, Widget FocusGroup)
setupDialog ui = do
  (dlg, fg) <- newDialog ui $ T.pack "Progress Bar Demo"
  dlg `onDialogAccept` const exitSuccess
  dlg `onDialogCancel` const exitSuccess
  return (dlg, fg)

main :: IO ()
main = do
  pb <- setupProgessBar
  setupProgressBarThread pb

  (dlg, dlgFg) <- setupDialog pb

  c <- newCollection
  _ <- addToCollection c (dialogWidget dlg) dlgFg

  runUi c $ defaultContext { normalAttr = bodyAttr
                           , focusAttr = focAttr
                           }
