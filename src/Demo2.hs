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
headerAttr = fgColor bright_green
msgAttr = fgColor blue

color :: Formatter
color = highlight (compile (BS8.pack "<.*>") []) (fgColor bright_green)

main :: IO ()
main = do
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles radio buttons and checkboxes\n\n\
            \- <ESC> quits"

      columns = [ column (Fixed 25) `pad` (padAll 1) `align` AlignRight
                , column Auto `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  tw <- (textWidget (wrap &.& color) msg) >>= withNormalAttribute msgAttr
  mainBox <- (return table) <--> (return tw) >>= withBoxSpacing 1

  ui <- centered =<< hLimit 70 mainBox

  r1 <- newCheckbox "ASCII"
  r2 <- newCheckbox "Unicode"
  r3 <- newCheckbox "Unicode Rounded"
  r4 <- newCheckbox "Unicode Bold"
  radioHeader <- simpleText "" >>= withNormalAttribute headerAttr

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2
  addToRadioGroup rg r3
  addToRadioGroup rg r4

  edit1 <- editWidget >>= withFocusAttribute (white `on` red)
  edit2 <- editWidget

  edit1Header <- simpleText "" >>= withNormalAttribute headerAttr
  edit2Header <- simpleText "" >>= withNormalAttribute headerAttr

  lst <- listWidget $ mkList focAttr simpleText

  selector <- vLimit 3 lst
  listHeader <- simpleText ""

  rs <- (return r1) <--> (return r2) <--> (return r3) <--> (return r4)

  addHeadingRow_ table headerAttr ["Column 1", "Column 2"]
  addRow table $ radioHeader .|. rs
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ customCell listHeader `align` AlignLeft .|. customCell selector `pad` padNone

  r1 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader "ASCII" >> setTableBorderSkin table asciiSkin

  r2 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader "Unicode" >> setTableBorderSkin table unicodeSkin

  r3 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader "Unicode Rounded" >> setTableBorderSkin table unicodeRoundedSkin

  r4 `onCheckboxChange` \_ v ->
      when v $ setText radioHeader "Unicode Bold" >> setTableBorderSkin table unicodeBoldSkin

  edit1 `onChange` \_ s -> setText edit1Header s
  edit2 `onChange` \_ s -> setText edit2Header s

  lst `onSelectionChange` \_ _ k _ ->
      setText listHeader $ "You selected: " ++ k

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setCheckboxChecked r2

  fgr <- newFocusGroup
  fgr `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

  mapM_ (addToList lst) [ "Foo"
                        , "Bar"
                        , "Baz"
                        , "Stuff"
                        , "Things"
                        ]

  addToFocusGroup fgr r1
  addToFocusGroup fgr r2
  addToFocusGroup fgr r3
  addToFocusGroup fgr r4
  addToFocusGroup fgr edit1
  addToFocusGroup fgr edit2
  addToFocusGroup fgr lst
  setFocusGroup ui fgr

  -- Enter the event loop.
  runUi ui $ defaultContext { focusAttr = focAttr
                            , normalAttr = fg `on` bg
                            }
