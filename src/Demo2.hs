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

      columns = [ column (Fixed 25) `pad` (padAll 1)
                , column Auto `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  tw <- (textWidget (wrap &.& color) msg) >>= withNormalAttribute msgAttr
  mainBox <- (return table) <--> (return tw) >>= withBoxSpacing 1

  ui <- centered =<< hLimit 70 mainBox

  r1 <- newCheckbox "Cake"
  r2 <- newCheckbox "Death"
  radioHeader <- simpleText "" >>= withNormalAttribute headerAttr

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2

  r3 <- newCheckbox "Frosting"

  edit1 <- editWidget >>= withFocusAttribute (white `on` red)
  edit2 <- editWidget

  edit1Header <- simpleText "" >>= withNormalAttribute headerAttr
  edit2Header <- simpleText "" >>= withNormalAttribute headerAttr

  lst <- listWidget =<< mkList (fgColor bright_green) simpleText

  selector <- vLimit 3 lst
  listHeader <- simpleText ""

  rs <- (return r1) <--> (return r2)

  addHeadingRow_ table headerAttr ["Column 1", "Column 2"]
  addRow table $ radioHeader .|. rs
  addRow table $ emptyCell .|. r3
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ listHeader .|. customCell selector `pad` padNone

  rg `onRadioChange` \cb -> do
      s <- getCheckboxLabel cb
      setText radioHeader $ s ++ ", please."

  edit1 `onChange` (setText edit1Header)
  edit2 `onChange` (setText edit2Header)

  lst `onSelectionChange` \ev ->
      case ev of
        SelectionOn _ k _ -> setText listHeader $ "You selected: " ++ k
        SelectionOff -> return ()

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setCheckboxChecked r1

  fgr <- newFocusGroup
  fgr `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

  mapM_ (addToList lst) [ "Cookies"
                        , "Cupcakes"
                        , "Twinkies"
                        , "M&Ms"
                        , "Fritos"
                        , "Cheetos"
                        ]

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