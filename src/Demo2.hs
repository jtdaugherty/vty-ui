{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All

-- Visual attributes.
borderAttr = blue `on` black
editAttr = white `on` black
editFocusAttr = bright_yellow `on` blue
bodyAttr = white `on` black
headerAttr = bright_yellow `on` black
msgAttr = bright_white `on` black

exitApp :: Vty -> IO a
exitApp vty = do
  reserve_display $ terminal vty
  shutdown vty
  exitSuccess

main :: IO ()
main = do
  vty <- mkVty

  let msg = "Press <TAB> to switch edit fields; press <ESC> to quit."

  table <- newTable borderAttr [Fixed 20, Fixed 20] BorderFull
  mainBox <- (return table)
          <--> (hLimit 43 =<< (textWidget wrap $ prepareText msgAttr msg))

  setBoxSpacing mainBox 2

  ui <- centered mainBox

  [col1Header, col2Header] <-
      addHeadingRow table headerAttr ["", ""]

  edit1 <- editWidget editAttr editFocusAttr
  edit2 <- editWidget editAttr editFocusAttr
  addRow table [ mkCell edit1, mkCell edit2 ]

  edit1 `onChange` \_ s -> setText col1Header headerAttr s
  edit2 `onChange` \_ s -> setText col2Header headerAttr s

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
