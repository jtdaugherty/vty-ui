{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Text.Regex.PCRE.Light
import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.ByteString.Char8 as BS8

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

color :: Formatter
color = highlight (compile (BS8.pack "<.*>") []) (bright_green `on` black)

main :: IO ()
main = do
  vty <- mkVty

  let msg = "<TAB> switches edit fields, ordinary \
            \keystrokes edit, <ESC> quits."

  table <- newTable borderAttr [Auto, Fixed 20, Auto] BorderFull
  mainBox <- (return table) <--> (textWidget (wrap &.& color) $ prepareText msgAttr msg)

  setBoxSpacing mainBox 2

  ui <- centered =<< hLimit 50 mainBox

  [_, col1Header, col2Header] <-
      addHeadingRow table headerAttr ["", "", ""]

  edit1 <- editWidget editAttr editFocusAttr
  edit2 <- editWidget editAttr editFocusAttr
  t <- simpleText bodyAttr "testing"

  addRow table [ mkCell t, mkCell edit1, mkCell edit2 ]

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
