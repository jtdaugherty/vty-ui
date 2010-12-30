{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Data.Maybe ( fromJust )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, modify, evalStateT )
import Text.Regex.PCRE.Light.Char8 ( Regex, compile )

import Graphics.Vty
    ( Event(..), Key(..), Vty, Attr
    , mkVty, shutdown, terminal, next_event, reserve_display
    , pic_for_image, update, with_fore_color, with_back_color
    , def_attr, blue, bright_white, bright_yellow, bright_green
    , black, yellow, red, terminal, display_bounds
    )
import Graphics.Vty.Widgets.Base
    ( (<-->)
    , (<++>)
    , hFill
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , render
    )
import Graphics.Vty.Widgets.Text
    ( Text, simpleText, wrap, highlight
    , prepareText, textWidget, (&.&)
    )
import Graphics.Vty.Widgets.Borders
    ( bordered, hBorder
    )
import Graphics.Vty.Widgets.Composed
    ( bottomPadded
    )
import Graphics.Vty.Widgets.List
    ( List, mkList, pageUp, pageDown, resize
    , scrollUp, scrollDown, listWidget, getSelected
    , selectedIndex
    )

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

boxAttr :: Attr
boxAttr = def_attr
            `with_back_color` black
            `with_fore_color` bright_yellow

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_green

selAttr :: Attr
selAttr = def_attr
           `with_back_color` yellow
           `with_fore_color` black

regex1 :: Regex
regex1 = compile "(to|an|or|too)" []

hlAttr1 :: Attr
hlAttr1 = def_attr
           `with_back_color` black
           `with_fore_color` red

regex2 :: Regex
regex2 = compile "(text|if|you)" []

hlAttr2 :: Attr
hlAttr2 = def_attr
           `with_back_color` black
           `with_fore_color` yellow

buildUi appst =
  let body = fromJust $ lookup (fst $ getSelected list) msgs
      currentItem = selectedIndex list + 1
      footer = (simpleText titleAttr $ " " ++ (show currentItem) ++ "/" ++ (show $ length msgs) ++ " ")
               <++> hFill titleAttr '-' 1
      msgs = theMessages appst
      list = theList appst
      formatter = wrap &.&
                  highlight regex1 hlAttr1 &.&
                  highlight regex2 hlAttr2
  in bordered boxAttr $ listWidget list
      <--> hBorder titleAttr
      <--> (bottomPadded $ textWidget formatter $ prepareText bodyAttr body)
      <--> footer

-- Construct the user interface based on the contents of the
-- application state.
uiFromState = buildUi <$> get

-- The application state; this encapsulates what can vary based on
-- user input and what is used to construct the interface.  This is a
-- place for widgets whose state need to be stored so they can be
-- modified and used to reconstruct the interface as input is handled
data AppState = AppState { theList :: List String Text
                         , theMessages :: [(String, String)]
                         }

scrollListUp :: AppState -> AppState
scrollListUp appst = appst { theList = scrollUp $ theList appst }

scrollListDown :: AppState -> AppState
scrollListDown appst = appst { theList = scrollDown $ theList appst }

pageListUp :: AppState -> AppState
pageListUp appst = appst { theList = pageUp $ theList appst }

pageListDown :: AppState -> AppState
pageListDown appst = appst { theList = pageDown $ theList appst }

resizeList :: Int -> AppState -> AppState
resizeList s appst = appst { theList = resize s $ theList appst }

-- Process events from VTY, possibly modifying the application state.
eventloop :: Vty
          -> StateT AppState IO (Widget a)
          -> (Event -> StateT AppState IO Bool)
          -> StateT AppState IO ()
eventloop vty uiBuilder handle = do
  w <- uiBuilder
  evt <- liftIO $ do
           sz <- display_bounds $ terminal vty
           -- XXX discarding new widget
           let img = fst $ render w sz
           update vty $ pic_for_image img
           next_event vty
  next <- handle evt
  if next then
      eventloop vty uiBuilder handle else
      return ()

continue :: StateT AppState IO Bool
continue = return True

stop :: StateT AppState IO Bool
stop = return False

handleEvent :: Event -> StateT AppState IO Bool
handleEvent (EvKey KUp []) = modify scrollListUp >> continue
handleEvent (EvKey KDown []) = modify scrollListDown >> continue
handleEvent (EvKey KPageUp []) = modify pageListUp >> continue
handleEvent (EvKey KPageDown []) = modify pageListDown >> continue
handleEvent (EvKey (KASCII 'q') []) = stop
handleEvent (EvResize _ h) = do
  let newSize = ceiling ((0.05 :: Double) * fromIntegral h)
  when (newSize > 0) $ modify (resizeList newSize)
  continue
handleEvent _ = continue

-- Construct the application state using the message map.
mkAppState :: [(String, String)] -> AppState
mkAppState messages =
    let list = mkList bodyAttr selAttr 5 labelWidgets
        labelWidgets = zip labels $ map mkWidget labels
        mkWidget = simpleText bodyAttr
        labels = map fst messages
    in AppState { theList = list
                , theMessages = messages
                }

main :: IO ()
main = do
  vty <- mkVty

  -- The data that we'll present in the interface.
  let messages = [ ("First", "This text is long enough that it will get wrapped \
                             \if you resize your terminal to something small. \
                             \It also contains enough text to get truncated at \
                             \the bottom if the display area is too small.\n\n\n" )
                 , ("Second", "the second message")
                 , ("Third", "the third message")
                 , ("Fourth", "the fourth message")
                 , ("Fifth", "the fifth message")
                 , ("Sixth", "the sixth message")
                 , ("Seventh", "the seventh message")
                 ]

  evalStateT (eventloop vty uiFromState handleEvent) $ mkAppState messages
  -- Clear the screen.
  reserve_display $ terminal vty
  shutdown vty
