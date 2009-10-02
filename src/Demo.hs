module Main where

import Data.Maybe ( fromJust )
import Control.Applicative ( (<$>) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, gets, modify, evalStateT )

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

-- Construct the user interface based on the contents of the
-- application state.
buildUi :: StateT AppState IO VBox
buildUi = do
  list <- gets theList
  msgs <- gets theMessages
  let body = fromJust $ lookup (getSelected list) msgs
      ui = list
           <--> hFill titleAttr '-' 1
           <--> wrappedText bodyAttr body
           <--> vFill bodyAttr ' '
           <--> footer
      footer = text titleAttr "- Status "
               <++> hFill titleAttr '-' 1

  return ui

-- The application state; this encapsulates what can vary based on
-- user input and what is used to construct the interface.  This is a
-- place for widgets whose state need to be stored so they can be
-- modified and used to reconstruct the interface as input is handled
data AppState = AppState { theList :: List
                         , theMessages :: [(String, String)]
                         }

-- Process events from VTY, possibly modifying the application state.
eventloop :: Vty -> StateT AppState IO ()
eventloop vty = do
  w <- buildUi
  evt <- liftIO $ do
                  pic_for_image <$> mkImage vty w >>= update vty
                  next_event vty
  case evt of
    -- If we got an up or down arrow key, modify the app state (list
    -- widget) and continue processing events.
    EvKey KUp [] -> do
                  modify (\appst -> appst { theList = scrollUp $ theList appst })
                  eventloop vty
    EvKey KDown [] -> do
                  modify (\appst -> appst { theList = scrollDown $ theList appst })
                  eventloop vty

    -- If we get 'q', quit.
    EvKey (KASCII 'q') [] -> return ()

    -- Any other key means keep looping (including terminal resize).
    _ -> eventloop vty

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

  evalStateT (eventloop vty) $ mkAppState messages
  -- Clear the screen.
  reserve_display $ terminal vty
  shutdown vty
