{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All

-- Visual attributes.
titleAttr = bright_white `on` blue
editAttr = white `on` black
editFocusAttr = bright_green `on` black
bodyAttr = bright_green `on` black
headerAttr = bright_yellow `on` black

exitApp :: Vty -> IO a
exitApp vty = do
  reserve_display $ terminal vty
  shutdown vty
  exitSuccess

main :: IO ()
main = do
  vty <- mkVty

  edit1 <- editWidget editAttr editFocusAttr
  edit2 <- editWidget editAttr editFocusAttr

  table <- newTable bodyAttr [Fixed 20, Fixed 20] BorderFull
  ui <- hCentered table

  [col1Header, col2Header] <-
      addHeadingRow table headerAttr ["", ""]

  addRow table [ mkCell edit1
               , mkCell edit2
               ]

  edit1 `onChange` \e -> do
         str <- getEditText e
         setText col1Header str headerAttr

  edit2 `onChange` \e -> do
         str <- getEditText e
         setText col2Header str headerAttr

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"

  fg <- newFocusGroup

  fg `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitApp vty
           _ -> return False

  addToFocusGroup_ fg edit1
  addToFocusGroup_ fg edit2
  setFocusGroup ui fg

  -- Enter the event loop.
  runUi vty ui
