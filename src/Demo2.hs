{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
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

color :: Formatter
color = highlight (compile (BS8.pack "<.*>") []) (bright_green `on` black)

main :: IO ()
main = do
  let msg = "<TAB> switches edit fields, ordinary \
            \keystrokes edit, <SPC> toggles radio \
            \button, <ESC> quits."

      specs = [ column (Fixed 25)
              , column Auto `pad` (padAll 1) `align` AlignRight
              ]

  table <- newTable borderAttr specs BorderFull

  tw <- textWidget (wrap &.& color) $ prepareText msgAttr msg
  mainBox <- (return table) <--> (return tw)

  setBoxSpacing mainBox 2

  ui <- centered =<< hLimit 70 mainBox

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

  lst <- listWidget $ mkList bodyAttr focusAttr (simpleText bodyAttr)

  selector <- vLimit 2 lst
  listHeader <- simpleText bodyAttr ""

  addHeadingRow_ table bodyAttr ["Foo", "Bar"]
  addRow table $ radioHeader .|. r1
  addRow table $ EmptyCell .|. r2
  addRow table $ r3Header .|. r3
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ listHeader .|. selector

  r1 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader headerAttr "radio 1 checked"

  r2 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader headerAttr "radio 2 checked"

  r3 `onCheckboxChange` \_ v ->
      setText r3Header headerAttr $ if v then "checked" else "unchecked"

  edit1 `onChange` \_ s -> setText edit1Header headerAttr s
  edit2 `onChange` \_ s -> setText edit2Header headerAttr s

  lst `onSelectionChange` \_ _ k _ ->
      setText listHeader bodyAttr $ "You selected: " ++ k

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setCheckboxChecked r1
  setCheckboxChecked r3

  fg <- newFocusGroup
  fg `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

  addToList lst "Foo"
  addToList lst "Bar"
  addToList lst "Baz"
  addToList lst "Stuff"
  addToList lst "Things"

  addToFocusGroup fg r1
  addToFocusGroup fg r2
  addToFocusGroup fg r3
  addToFocusGroup fg edit1
  addToFocusGroup fg edit2
  addToFocusGroup fg lst
  setFocusGroup ui fg

  -- Enter the event loop.
  runUi ui
