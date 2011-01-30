{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

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
          KEnter -> act
          _ -> return ()
        return False

button :: (MonadIO m) => String -> m Button
button msg = do
  w <- simpleText msg >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  return $ Button msg w

data Dialog = Dialog { okButton :: Button
                     , cancelButton :: Button
                     , dialogWidget :: Widget (VCentered (HCentered Padded))
                     , setDialogTitle :: String -> IO ()
                     }

dialog :: (MonadIO m, Show a) => Widget a -> String -> Maybe (Widget FocusGroup)
       -> m Dialog
dialog body title mFg = do
  okB <- button "OK"
  cancelB <- button "Cancel"

  buttonBox <- (return $ buttonWidget okB) <++> (return $ buttonWidget cancelB)
  setBoxSpacing buttonBox 4

  b <- (hCentered body) <--> (hCentered buttonBox) >>= withBoxSpacing 1
  b2 <- padded b (padTopBottom 1)

  fg <- case mFg of
          Just g -> return g
          Nothing -> newFocusGroup

  addToFocusGroup fg $ buttonWidget okB
  addToFocusGroup fg $ buttonWidget cancelB

  b <- bordered b2 >>=
       withBorderedLabel title >>=
       withNormalAttribute (white `on` blue)

  c <- centered =<< withPadding (padLeftRight 10) b

  setFocusGroup c fg
  return $ Dialog { okButton = okB
                  , cancelButton = cancelB
                  , dialogWidget = c
                  , setDialogTitle = setBorderedLabel b
                  }

main :: IO ()
main = do
  e <- editWidget
  fg <- newFocusGroup
  addToFocusGroup fg e

  u <- (simpleText "Enter some text and press enter.") <--> (return e) >>= withBoxSpacing 1

  pe <- padded u (padLeftRight 2)
  d <- dialog pe "<enter text>" (Just fg)

  let updateTitle = setDialogTitle d =<< getEditText e

  (okButton d) `onButtonPressed` updateTitle
  e `onChange` \_ _ -> updateTitle

  (cancelButton d) `onButtonPressed` exitSuccess

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi (dialogWidget d) $ defaultContext { focusAttr = black `on` yellow }
