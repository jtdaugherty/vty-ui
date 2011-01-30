{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All

-- Dialog: message, array of "button" information. Buttons can be
-- tabbed-between and activated, and each one has a handler.

-- Another type of dialog would be an edit dialog.  (msg <-->
-- editField)

-- General case: dialog embeds any widget, and the dialog
-- automatically adds it to the focus group (at the beginning) if it
-- is focusable.  Don't have a way to determine whether it is
-- focusable; this would be another nice case of a Focusable type
-- class.  But then again, runtime detection of focus support would
-- allow us to make it optional, and embed any widget at all (like
-- text).

-- A "button" is a bit of text with left- and right-padding (Padded
-- Button) which can get focused

button :: (MonadIO m) => String -> IO () -> m (Widget Padded)
button msg activateHandler = do
  t <- simpleText msg
  b <- padded t (padLeft 3 `pad` padRight 3)
  b `onKeyPressed` \_ k _ -> do
    case k of
      KEnter -> activateHandler >> return True
      _ -> return False
  (return b) >>=
    withNormalAttribute (white `on` black) >>=
    withFocusAttribute (blue `on` white)

dialog body handler mFg = do
  okButton <- button "OK" (handler "OK")
  cancelButton <- button "Cancel" (handler "Cancel")

  buttonBox <- (return okButton) <++> (return cancelButton)
  setBoxSpacing buttonBox 4

  b <- (hCentered body) <--> (hCentered buttonBox) >>= withBoxSpacing 1
  b2 <- padded b (padTopBottom 1)

  fg <- case mFg of
          Just g -> return g
          Nothing -> newFocusGroup

  addToFocusGroup fg okButton
  addToFocusGroup fg cancelButton

  c <- centered =<< withPadding (padLeftRight 10) =<< (bordered b2 >>= withNormalAttribute (white `on` blue))
  setFocusGroup c fg
  return c

main :: IO ()
main = do
  e <- editWidget
  fg <- newFocusGroup
  addToFocusGroup fg e

  pe <- padded e (padLeftRight 2)
  d <- dialog pe (const $ return ()) (Just fg)

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi d $ defaultContext { focusAttr = black `on` yellow }
