{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import System.Exit ( exitSuccess )
import System.Locale
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import Data.Time.Format
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

data FrostingType = Chocolate
                  | Vanilla
                  | Lemon
                    deriving (Eq, Show)

main :: IO ()
main = do
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles radio buttons and checkboxes\n\n\
            \- <ESC> quits"

      columns = [ column (ColFixed 25) `pad` (padAll 1)
                , column ColAuto `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  tw <- (textWidget (wrap &.& color) msg) >>= withNormalAttribute msgAttr
  mainBox <- vBox table tw >>= withBoxSpacing 1

  r1 <- newCheckbox "Cake"
  r2 <- newCheckbox "Death"
  radioHeader <- simpleText "" >>= withNormalAttribute headerAttr

  rg <- newRadioGroup
  addToRadioGroup rg r1
  addToRadioGroup rg r2

  r3 <- newMultiStateCheckbox "Frosting" [ (Chocolate, 'C')
                                         , (Vanilla, 'V')
                                         , (Lemon, 'L')
                                         ]

  edit1 <- editWidget >>= withFocusAttribute (white `on` red)
  edit2 <- editWidget

  edit1Header <- textWidget wrap "" >>= withNormalAttribute headerAttr
  edit2Header <- textWidget wrap "" >>= withNormalAttribute headerAttr

  lst <- newListWidget =<< newList (fgColor bright_green) simpleText

  selector <- vLimit 3 lst
  listHeader <- simpleText ""

  rs <- vBox r1 r2

  cbHeader <- simpleText ""
  timeText <- simpleText ""

  addHeadingRow_ table headerAttr ["Column 1", "Column 2"]
  addRow table $ radioHeader .|. rs
  addRow table $ cbHeader .|. r3
  addRow table $ edit1Header .|. edit1
  addRow table $ edit2Header .|. edit2
  addRow table $ listHeader .|. customCell selector `pad` padNone
  addRow table $ emptyCell .|. timeText

  rg `onRadioChange` \cb -> do
      s <- getCheckboxLabel cb
      setText radioHeader $ s ++ ", please."

  r3 `onCheckboxChange` \v ->
      setText cbHeader $ "you chose: " ++ show v

  edit1 `onChange` (setText edit1Header)
  edit2 `onChange` (setText edit2Header)

  lst `onSelectionChange` \ev ->
      case ev of
        SelectionOn _ k _ -> setText listHeader $ "You selected: " ++ k
        SelectionOff -> return ()

  setEditText edit1 "Foo"
  setEditText edit2 "Bar"
  setCheckboxChecked r1

  setCheckboxState r3 Chocolate
  -- It would be nice if we didn't have to do this, but the
  -- setCheckboxState call above will not notify any state-change
  -- handlers because the state isn't actually changing (from its
  -- original value of Chocolate, the first value in its state list).
  setText cbHeader $ "you chose: Chocolate"

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

  ui <- centered =<< hLimit 70 mainBox
  setFocusGroup ui fgr

  forkIO $ forever $ do
         schedule $ do
           t <- getCurrentTime
           setText timeText $ formatTime defaultTimeLocale rfc822DateFormat t
         threadDelay $ 1 * 1000 * 1000

  -- Enter the event loop.
  runUi ui $ defaultContext { focusAttr = focAttr
                            , normalAttr = fg `on` bg
                            }