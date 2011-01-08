{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, evalStateT )

import Graphics.Vty
import Graphics.Vty.Widgets.All

-- The application state; this contains references to widgets that
-- need to be updated when events occur.
data AppState =
    AppState { theList :: Widget (List String FormattedText)
             , theMessages :: [(String, String)]
             , theBody :: Widget FormattedText
             , theFooter :: Widget FormattedText
             , uis :: Widget Collection
             }

on :: Color -> Color -> Attr
on a b = def_attr `with_back_color` b `with_fore_color` a

-- Visual attributes.
titleAttr = bright_white `on` blue
boxAttr = bright_yellow `on` black
bodyAttr = bright_green `on` black
selAttr = black `on` yellow
hlAttr1 = red `on` black
hlAttr2 = yellow `on` black

-- The data that we'll present in the interface.
messages :: [(String, String)]
messages = [ ("First", "This text is long enough that it will get wrapped \
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

buildUi1 appst = do
  (hBorder titleAttr)
      <--> (bottomPadded (theList appst) bodyAttr)
      <--> ((return $ theFooter appst) <++> hBorder titleAttr)

buildUi2 appst = do
  (hBorder titleAttr)
      <--> (return $ theList appst)
      <--> (hBorder titleAttr)
      <--> (bottomPadded (theBody appst) bodyAttr)
      <--> ((return $ theFooter appst) <++> hBorder titleAttr)

-- Process events from VTY, possibly modifying the application state.
eventloop :: Vty
          -> Widget a
          -> StateT AppState IO ()
eventloop vty w = do
  -- XXX shouldn't be here
  updateUiFromState
  evt <- liftIO $ do
           sz <- display_bounds $ terminal vty
           img <- render w (DisplayRegion 0 0) sz Nothing
           update vty $ pic_for_image img
           next_event vty

  case evt of
    (EvKey k _) -> handleKeyEvent w k >> return ()
    _ -> return ()

  eventloop vty w

updateUiFromState :: StateT AppState IO ()
updateUiFromState = do
  appst <- get
  (i, _) <- getSelected $ theList appst
  setText (theBody appst) (snd $ theMessages appst !! i) bodyAttr

  let msg = " " ++ (show $ i + 1) ++ "/" ++ (show $ length $ theMessages appst) ++ " "
  setText (theFooter appst) msg titleAttr

-- Construct the application state using the message map.
mkAppState :: IO AppState
mkAppState = do
  let labels = map fst messages

  lw <- listWidget =<< mkSimpleList bodyAttr selAttr 5 labels
  b <- simpleText bodyAttr ""
  f <- simpleText titleAttr ""

  c <- newCollection

  return $ AppState { theList = lw
                    , theMessages = messages
                    , theBody = b
                    , theFooter = f
                    , uis = c
                    }

main :: IO ()
main = do
  vty <- mkVty

  st <- mkAppState

  ui1 <- buildUi1 st
  ui2 <- buildUi2 st

  addToCollection (uis st) ui1
  addToCollection (uis st) ui2

  let exitApp = liftIO $ do
                  reserve_display $ terminal vty
                  shutdown vty
                  exitSuccess

      universalKeys =
          \_ k -> do
            case k of
              (KASCII 'q') -> exitApp
              _ -> return False

  ui1 `onKeyPressed` \_ k -> do
         case k of
           KEnter -> setCurrent (uis st) 1 >> return True
           _ -> return False

  ui2 `onKeyPressed` \_ k -> do
         case k of
           KEsc -> setCurrent (uis st) 0 >> return True
           (KASCII 'w') -> setCurrent (uis st) 0 >> return True
           _ -> return False

  (uis st) `onKeyPressed` universalKeys

  -- Enter the event loop.
  evalStateT (eventloop vty (uis st)) st
