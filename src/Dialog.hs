{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import System.Exit
import Graphics.Vty hiding (Button)
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

data Button = Button { buttonText :: String
                     , buttonWidget :: Widget Padded
                     }

onButtonPressed :: (MonadIO m) => Button -> IO () -> m ()
onButtonPressed b act = do
  (buttonWidget b) `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> act >> return False
          _ -> return False

button :: (MonadIO m) => String -> m Button
button msg = do
  t <- simpleText msg
  b <- padded t (padLeft 3 `pad` padRight 3)
  w <- (return b) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  return $ Button msg w

data Dialog = Dialog { okButton :: Button
                     , cancelButton :: Button
                     , dialogWidget :: Widget (VCentered (HCentered Padded))
                     }

dialog :: (MonadIO m, Show a) => Widget a -> Maybe (Widget FocusGroup)
       -> m Dialog
dialog body mFg = do
  okButton <- button "OK"
  cancelButton <- button "Cancel"

  buttonBox <- (return $ buttonWidget okButton) <++> (return $ buttonWidget cancelButton)
  setBoxSpacing buttonBox 4

  b <- (hCentered body) <--> (hCentered buttonBox) >>= withBoxSpacing 1
  b2 <- padded b (padTopBottom 1)

  fg <- case mFg of
          Just g -> return g
          Nothing -> newFocusGroup

  addToFocusGroup fg $ buttonWidget okButton
  addToFocusGroup fg $ buttonWidget cancelButton

  c <- centered =<<
       withPadding (padLeftRight 10) =<<
       (bordered b2 >>= withNormalAttribute (white `on` blue))

  setFocusGroup c fg
  return $ Dialog { okButton = okButton
                  , cancelButton = cancelButton
                  , dialogWidget = c
                  }

main :: IO ()
main = do
  e <- editWidget
  fg <- newFocusGroup
  addToFocusGroup fg e

  pe <- padded e (padLeftRight 2)
  d <- dialog pe (Just fg)

  let done = putStrLn =<< getEditText e

  (okButton d) `onButtonPressed` done
  e `onActivate` const done

  (cancelButton d) `onButtonPressed` exitSuccess

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi (dialogWidget d) $ defaultContext { focusAttr = black `on` yellow }
