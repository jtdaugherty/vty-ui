{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, evalStateT, gets )
import Text.Regex.PCRE.Light.Char8 ( Regex, compile )

import Graphics.Vty
    ( Event(..), Key(..), Vty, Attr, Color
    , mkVty, shutdown, terminal, next_event, reserve_display
    , pic_for_image, update, with_fore_color, with_back_color
    , def_attr, blue, bright_white, bright_yellow, bright_green
    , black, yellow, red, terminal, display_bounds
    )
import Graphics.Vty.Widgets.All
    ( (<-->)
    , (<++>)
    , (&.&)
    , Widget, FormattedText, List
    , render
    , prepareText, simpleText, wrap, highlight
    , textWidget, setText, hBorder
    , bottomPadded, mkSimpleList, pageUp, pageDown, resize
    , scrollUp, scrollDown, listWidget, getSelected
    )

-- The application state; this contains references to widgets that
-- need to be updated when events occur.
data AppState =
    AppState { theList :: Widget (List String FormattedText)
             , theMessages :: [(String, String)]
             , theBody :: Widget FormattedText
             , theFooter :: Widget FormattedText
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

regex1 :: Regex
regex1 = compile "(to|an|or|too)" []

regex2 :: Regex
regex2 = compile "(text|if|you)" []

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

buildUi appst = do
  footer <- (return $ theFooter appst) <++> hBorder titleAttr

  (hBorder titleAttr)
      <--> (return $ theList appst)
      <--> (hBorder titleAttr)
      <--> (bottomPadded (theBody appst) bodyAttr)
      <--> (return footer)

scrollListUp :: StateT AppState IO ()
scrollListUp = gets theList >>= scrollUp

scrollListDown :: StateT AppState IO ()
scrollListDown = gets theList >>= scrollDown

pageListUp :: StateT AppState IO ()
pageListUp = gets theList >>= pageUp

pageListDown :: StateT AppState IO ()
pageListDown = gets theList >>= pageDown

resizeList :: Int -> StateT AppState IO ()
resizeList s = gets theList >>= (resize s)

-- Process events from VTY, possibly modifying the application state.
eventloop :: Vty
          -> Widget a
          -> (Event -> StateT AppState IO Bool)
          -> StateT AppState IO ()
eventloop vty w handle = do
  evt <- liftIO $ do
           sz <- display_bounds $ terminal vty
           img <- render w sz Nothing
           update vty $ pic_for_image img
           next_event vty
  next <- handle evt
  if next then
      eventloop vty w handle else
      return ()

continue :: StateT AppState IO Bool
continue = return True

stop :: StateT AppState IO Bool
stop = return False

handleEvent :: Event -> StateT AppState IO Bool
handleEvent (EvKey KUp []) = scrollListUp >> updateBody >> continue
handleEvent (EvKey KDown []) = scrollListDown >> updateBody >> continue
handleEvent (EvKey KPageUp []) = pageListUp >> updateBody >> continue
handleEvent (EvKey KPageDown []) = pageListDown >> updateBody >> continue
handleEvent (EvKey (KASCII 'q') []) = stop
handleEvent (EvResize _ h) = do
  let newSize = ceiling ((0.05 :: Double) * fromIntegral h)
  when (newSize > 0) $ resizeList newSize
  continue
handleEvent _ = continue

updateBody :: StateT AppState IO ()
updateBody = do
  appst <- get
  (i, _) <- getSelected $ theList appst
  setText (theBody appst) (snd $ theMessages appst !! i) bodyAttr
  updateFooter

updateFooter :: StateT AppState IO ()
updateFooter = do
  appst <- get
  (i, _) <- getSelected $ theList appst
  let msg = " " ++ (show $ i + 1) ++ "/" ++ (show $ length $ theMessages appst) ++ " "
  setText (theFooter appst) msg titleAttr

-- Construct the application state using the message map.
mkAppState :: IO AppState
mkAppState = do
  let labels = map fst messages
  lw <- listWidget =<< mkSimpleList bodyAttr selAttr 5 labels

  let formatter = wrap &.&
                  highlight regex1 hlAttr1 &.&
                  highlight regex2 hlAttr2

  b <- textWidget formatter $ prepareText bodyAttr ""
  f <- simpleText titleAttr ""

  return $ AppState { theList = lw
                    , theMessages = messages
                    , theBody = b
                    , theFooter = f
                    }

main :: IO ()
main = do
  vty <- mkVty

  st <- mkAppState
  w <- buildUi st

  -- Perform initial interface setup and enter the event loop.
  evalStateT (updateBody >> updateFooter >> eventloop vty w handleEvent) st

  -- Clear the screen.
  reserve_display $ terminal vty
  shutdown vty
