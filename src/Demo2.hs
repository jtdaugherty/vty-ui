{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import System.Exit ( exitSuccess )
import Control.Monad ( when )
import Text.Regex.PCRE.Light
import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.ByteString.Char8 as BS8

-- Visual attributes.
fg = white
bg = black

focAttr = black `on` yellow
bodyAttr = blue `on` bg
headerAttr = bright_yellow `on` bg
msgAttr = blue `on` bg

color :: Formatter
color = highlight (compile (BS8.pack "<.*>") []) (bright_green `on` bg)

main :: IO ()
main = do
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles radio buttons and checkboxes\n\n\
            \- <ESC> quits"

      columns = [ column (Fixed 25) `pad` (padAll 1) `align` AlignRight
                , column Auto `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull
  setNormalAttribute table (white `on` blue)
  setBorderAttribute table (green `on` blue)

  tw <- textWidget (wrap &.& color) $ prepareText msgAttr msg
  mainBox <- (return table) <--> (return tw)

  setBoxSpacing mainBox 1

  ui <- centered =<< hLimit 70 mainBox

  r1 <- newCheckbox "Cake"
  r2 <- newCheckbox "Death"
  r3 <- newCheckbox "Checkbox"
  radioHeader <- simpleText headerAttr ""
  r3Header <- simpleText headerAttr ""

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2

  edit1 <- editWidget
  setFocusAttribute edit1 (white `on` red)

  edit2 <- editWidget

  edit1Header <- simpleText headerAttr ""
  edit2Header <- simpleText headerAttr ""

  lst <- listWidget $ mkList focAttr (simpleText bodyAttr)

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

  fgr <- newFocusGroup
  fgr `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

  addToList lst "Foo"
  addToList lst "Bar"
  addToList lst "Baz"
  addToList lst "Stuff"
  addToList lst "Things"

  addToFocusGroup fgr r1
  addToFocusGroup fgr r2
  addToFocusGroup fgr r3
  addToFocusGroup fgr edit1
  addToFocusGroup fgr edit2
  addToFocusGroup fgr lst
  setFocusGroup ui fgr

  -- Enter the event loop.
  runUi ui $ defaultContext { focusAttr = focAttr
                            , normalAttr = fg `on` bg
                            }
