module Main where

import Data.Maybe ( fromJust )
import Control.Applicative ( (<$>) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, modify, evalStateT )

import Graphics.Vty
import Graphics.Vty.Widgets.All

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_green

selAttr :: Attr
selAttr = def_attr
           `with_back_color` yellow
           `with_fore_color` black

buildUi :: AppState -> VBox
buildUi appst =
  let body = fromJust $ lookup (getSelected list) msgs
      footer = text titleAttr "- Status "
               <++> hFill titleAttr '-' 1
      msgs = theMessages appst
      list = theList appst
  in list
      <--> hBorder titleAttr
      <--> (bottomPadded (wrappedText bodyAttr body))
      <--> footer

-- Construct the user interface based on the contents of the
-- application state.
uiFromState :: StateT AppState IO VBox
uiFromState = buildUi <$> get

-- The application state; this encapsulates what can vary based on
-- user input and what is used to construct the interface.  This is a
-- place for widgets whose state need to be stored so they can be
-- modified and used to reconstruct the interface as input is handled
data AppState = AppState { theList :: List
                         , theMessages :: [(String, String)]
                         }

scrollListUp :: StateT AppState IO ()
scrollListUp = modify (\appst -> appst { theList = scrollUp $ theList appst })

scrollListDown :: StateT AppState IO ()
scrollListDown = modify (\appst -> appst { theList = scrollDown $ theList appst })

-- Process events from VTY, possibly modifying the application state.
eventloop :: (Widget a) => Vty
          -> StateT AppState IO a
          -> (Event -> StateT AppState IO Bool)
          -> StateT AppState IO ()
eventloop vty uiBuilder handle = do
  w <- uiBuilder
  evt <- liftIO $ do
                  pic_for_image <$> mkImage vty w >>= update vty
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
handleEvent (EvKey KUp []) = scrollListUp >> continue
handleEvent (EvKey KDown []) = scrollListDown >> continue
handleEvent (EvKey (KASCII 'q') []) = stop
handleEvent _ = continue

-- Construct the application state using the message map.
mkAppState :: [(String, String)] -> AppState
mkAppState messages =
    let list = mkList bodyAttr selAttr 3 $ map fst messages
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
                             \the bottom if the display area is too small." )
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
