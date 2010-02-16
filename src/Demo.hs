module Main where

import Data.Maybe ( fromJust )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, modify, evalStateT )

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Text.Regex.PCRE.Light.Char8 ( Regex, compile )

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

urlRegex :: Regex
urlRegex = compile "https?:\\/\\/([^\\.]+)(\\.[^\\.]+)+(\\:\\d+)?(\\/[\\-\\/#\\?\\&\\;a-zA-Z\\%0-9_]*)?$" []

urlAttr :: Attr
urlAttr = def_attr
           `with_back_color` yellow
           `with_fore_color` red

buildUi :: AppState -> Widget
buildUi appst =
  let body = fromJust $ lookup (fst $ getSelected list) msgs
      currentItem = selectedIndex list + 1
      footer = (simpleText titleAttr $ " " ++ (show currentItem) ++ "/" ++ (show $ length msgs) ++ " ")
               <++> hFill titleAttr '-' 1
      msgs = theMessages appst
      list = theList appst
  in bordered boxAttr $ listWidget list
      <--> hBorder titleAttr
      <--> (bottomPadded $ textWidget [wrap, highlight urlRegex urlAttr] $ prepareText bodyAttr body)
      <--> footer

-- Construct the user interface based on the contents of the
-- application state.
uiFromState :: StateT AppState IO Widget
uiFromState = buildUi <$> get

-- The application state; this encapsulates what can vary based on
-- user input and what is used to construct the interface.  This is a
-- place for widgets whose state need to be stored so they can be
-- modified and used to reconstruct the interface as input is handled
data AppState = AppState { theList :: List String
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
          -> StateT AppState IO Widget
          -> (Event -> StateT AppState IO Bool)
          -> StateT AppState IO ()
eventloop vty uiBuilder handle = do
  w <- uiBuilder
  evt <- liftIO $ do
           (img, _) <- mkImage vty w
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
  let newSize = ceiling (0.05 * fromIntegral h)
  when (newSize > 0) $ modify (resizeList newSize)
  continue
handleEvent _ = continue

-- Construct the application state using the message map.
mkAppState :: [(String, String)] -> AppState
mkAppState messages =
    let list = mkList bodyAttr selAttr 3 borederedLabels
        borederedLabels = zip labels $ map mkWidget labels
        mkWidget = bordered boxAttr . simpleText bodyAttr
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
                             \It also contains enough text to get truncated (a url looks like http://www.google.com/) at \
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
