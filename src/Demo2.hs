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
  let msg = "<TAB> switches edit fields, ordinary \
            \keystrokes edit, <SPC> toggles radio \
            \button, <ESC> quits."

  table <- newTable borderAttr [Fixed 20, Auto] BorderFull
  tw <- textWidget (wrap &.& color) $ prepareText msgAttr msg
  mainBox <- (return table) <--> (return tw)

  setBoxSpacing mainBox 2

  ui <- centered =<< hLimit 50 mainBox

  r1 <- newCheckbox "Cb 1" bodyAttr focusAttr
  r2 <- newCheckbox "Cb 2" bodyAttr focusAttr
  r3 <- newCheckbox "Cb 3 (no radio)" bodyAttr focusAttr
  radioHeader <- simpleText headerAttr ""
  r3Header <- simpleText headerAttr ""

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2

  edit1 <- editWidget editAttr focusAttr
  edit1Header <- simpleText headerAttr ""

  edit2 <- editWidget editAttr focusAttr
  edit2Header <- simpleText headerAttr ""

  b <- (simpleText bodyAttr "Foo") <--> (simpleText bodyAttr "Bar")

  addHeadingRow_ table bodyAttr ["Foo", "Bar"]
  addRow table $ radioHeader .|. r1
  addRow table $ EmptyCell .|. r2
  addRow table $ r3Header .|. r3
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ EmptyCell .|. b
  addRow table $ EmptyCell .|. tw

  r1 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader headerAttr "radio 1 checked"

  r2 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader headerAttr "radio 2 checked"

  r3 `onCheckboxChange` \_ v ->
      setText r3Header headerAttr $ if v then "checked" else "unchecked"

  edit1 `onChange` \_ s -> setText edit1Header headerAttr s
  edit2 `onChange` \_ s -> setText edit2Header headerAttr s

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setCheckboxChecked r1
  setCheckboxChecked r3

  vty <- mkVty

  fg <- newFocusGroup
  fg `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitApp vty
           _ -> return False

  addToFocusGroup_ fg r1
  addToFocusGroup_ fg r2
  addToFocusGroup_ fg r3
  addToFocusGroup_ fg edit1
  addToFocusGroup_ fg edit2
  setFocusGroup ui fg

  -- Enter the event loop.
  runUi vty ui
