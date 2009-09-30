module Main where

import Data.Maybe ( fromJust )

import Control.Applicative ( (<$>) )
import Control.Monad ( forM_ )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, put, get, gets, evalStateT )

import Graphics.Vty.Widgets.Base
import Graphics.Vty.Widgets.List

import Graphics.Vty

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

buildUi :: StateT AppState IO VBox
buildUi = do
  list <- gets theList
  msgs <- gets theMessages
  let body = fromJust $ lookup (getSelected list) msgs
      bodyArea = VBox
                 (HFill titleAttr '-' 1)
                 (VBox
                  (Text bodyAttr body)
                  (VFill bodyAttr ' ')
                 )
      footer = HBox
               (Text titleAttr "- Status -")
               (HFill titleAttr '-' 1)

  return $ VBox (VBox list bodyArea) footer

data AppState = AppState { theList :: List
                         , theMessages :: [(String, String)]
                         }

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
                  appst <- get
                  put (appst { theList = scrollUp $ theList appst })
                  eventloop vty
    EvKey KDown [] -> do
                  appst <- get
                  put (appst { theList = scrollDown $ theList appst })
                  eventloop vty

    -- If we get 'q', quit.
    EvKey (KASCII 'q') [] -> return ()

    -- Any other key means keep looping (including terminal resize).
    _ -> eventloop vty

mkAppState :: [(String, String)] -> AppState
mkAppState messages =
    let list = mkList bodyAttr selAttr 3 $ map fst messages
    in AppState { theList = list
                , theMessages = messages
                }

main :: IO ()
main = do
  vty <- mkVty

  -- Set up the initial app state.
  let messages = [ ("First", "the first message")
                 , ("Second", "the second message")
                 , ("Third", "the third message")
                 , ("Fourth", "the fourth message")
                 , ("Fifth", "the fifth message")
                 , ("Sixth", "the sixth message")
                 , ("Seventh", "the seventh message")
                 ]

  evalStateT (eventloop vty) $ mkAppState messages
  shutdown vty
