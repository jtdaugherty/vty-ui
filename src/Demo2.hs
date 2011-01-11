{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Control.Monad ( when )
import Text.Regex.PCRE.Light
import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.ByteString.Char8 as BS8

-- Visual attributes.
borderAttr = blue `on` black
editAttr = white `on` black
focusAttr = bright_yellow `on` blue
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
            \keystrokes edit, <SPC> toggles radio \
            \button, <ESC> quits."

  table <- newTable borderAttr [Fixed 20, Auto] BorderFull
  mainBox <- (return table) <--> (textWidget (wrap &.& color) $ prepareText msgAttr msg)

  setBoxSpacing mainBox 2

  ui <- centered =<< hLimit 50 mainBox

  r1 <- newRadio "Radio 1" bodyAttr focusAttr
  r2 <- newRadio "Radio 2" bodyAttr focusAttr
  radioHeader <- simpleText headerAttr ""

  edit1 <- editWidget editAttr focusAttr
  edit1Header <- simpleText headerAttr ""

  edit2 <- editWidget editAttr focusAttr
  edit2Header <- simpleText headerAttr ""

  e <- simpleText bodyAttr ""

  addRow table [ mkCell radioHeader, mkCell r1 ]
  addRow table [ mkCell e, mkCell r2 ]
  addRow table [ mkCell edit1Header, mkCell edit1 ]
  addRow table [ mkCell edit2Header, mkCell edit2 ]

  r1 `onRadioChange` \_ v -> when v $ setText radioHeader headerAttr "radio 1 checked"
  r2 `onRadioChange` \_ v -> when v $ setText radioHeader headerAttr "radio 2 checked"

  edit1 `onChange` \_ s -> setText edit1Header headerAttr s
  edit2 `onChange` \_ s -> setText edit2Header headerAttr s

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setRadioChecked r1
  setRadioUnchecked r2

  fg <- newFocusGroup
  fg `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitApp vty
           _ -> return False

  addToFocusGroup_ fg r1
  addToFocusGroup_ fg r2
  addToFocusGroup_ fg edit1
  addToFocusGroup_ fg edit2
  setFocusGroup ui fg

  -- Enter the event loop.
  runUi vty ui
