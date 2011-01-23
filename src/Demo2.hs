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
focusAttr = black `on` yellow
bodyAttr = white `on` black
headerAttr = bright_yellow `on` black
msgAttr = bright_white `on` black

color :: Formatter
color = highlight (compile (BS8.pack "<.*>") []) (bright_green `on` black)

main :: IO ()
main = do
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles radio buttons and checkboxes\n\n\
            \- <ESC> quits"

      columns = [ column (Fixed 25) `pad` (padAll 1) `align` AlignRight
                , column Auto `pad` (padAll 1)
                ]

  table <- newTable borderAttr columns BorderFull

  tw <- textWidget (wrap &.& color) $ prepareText msgAttr msg
  mainBox <- (return table) <--> (return tw)

  setBoxSpacing mainBox 1

  ui <- centered =<< hLimit 70 mainBox

  r1 <- newCheckbox "Cake" focusAttr
  r2 <- newCheckbox "Death" focusAttr
  r3 <- newCheckbox "Checkbox" focusAttr
  radioHeader <- simpleText headerAttr ""
  r3Header <- simpleText headerAttr ""

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2

  edit1 <- editWidget focusAttr
  edit2 <- editWidget focusAttr

  edit1Header <- simpleText headerAttr ""
  edit2Header <- simpleText headerAttr ""

  lst <- listWidget $ mkList focusAttr (simpleText bodyAttr)

  selector <- vLimit 3 lst
  listHeader <- simpleText bodyAttr ""

  rs <- (return r1) <--> (return r2)

  addHeadingRow_ table headerAttr ["Column 1", "Column 2"]
  addRow table $ (radioHeader .|.) rs
  addRow table $ r3Header .|. r3
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ customCell listHeader `align` AlignLeft .|. customCell selector `pad` padNone

  r1 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader bodyAttr "CAKE!!"

  r2 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader bodyAttr "death..."

  r3 `onCheckboxChange` \_ v ->
      setText r3Header bodyAttr $ if v then "checked" else "unchecked"

  edit1 `onChange` \_ s -> setText edit1Header bodyAttr s
  edit2 `onChange` \_ s -> setText edit2Header bodyAttr s

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
  runUi ui (yellow `on` blue)
