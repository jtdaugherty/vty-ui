{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Graphics.Vty.Widgets.Dialog
    ( Dialog(okButton, cancelButton, dialogWidget, setDialogTitle)
    , newDialog
    , onDialogAccept
    , onDialogCancel
    , acceptDialog
    , cancelDialog
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Data.IORef
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Centering
import Graphics.Vty.Widgets.Button
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Core
import Graphics.Vty hiding (Button)

data DialogEvent = DialogAccept
                 | DialogCancel
                   deriving (Eq)

data Dialog = Dialog { okButton :: Button
                     , cancelButton :: Button
                     , dialogWidget :: Widget (VCentered (HCentered Padded))
                     , setDialogTitle :: String -> IO ()
                     , dialogAcceptHandlers :: IORef [Handler Dialog]
                     , dialogCancelHandlers :: IORef [Handler Dialog]
                     }

newDialog :: (MonadIO m, Show a) => Widget a -> String -> Maybe (Widget FocusGroup)
          -> m Dialog
newDialog body title mFg = do
  okB <- newButton "OK"
  cancelB <- newButton "Cancel"

  buttonBox <- (return $ buttonWidget okB) <++> (return $ buttonWidget cancelB)
  setBoxSpacing buttonBox 4

  b <- withPadding (padTopBottom 1) =<<
       ((hCentered body) <--> (hCentered buttonBox) >>= withBoxSpacing 1)

  fg <- case mFg of
          Just g -> return g
          Nothing -> newFocusGroup

  addToFocusGroup fg $ buttonWidget okB
  addToFocusGroup fg $ buttonWidget cancelB

  -- XXX don't hard-code attributes in this module; give dialog a
  -- normal attribute??
  b2 <- bordered b >>=
        withBorderedLabel title >>=
        withNormalAttribute (white `on` blue)

  c <- centered =<< withPadding (padLeftRight 10) b2

  setFocusGroup c fg

  ahs <- mkHandlers
  chs <- mkHandlers

  let dlg = Dialog { okButton = okB
                   , cancelButton = cancelB
                   , dialogWidget = c
                   , setDialogTitle = setBorderedLabel b2
                   , dialogAcceptHandlers = ahs
                   , dialogCancelHandlers = chs
                   }

  okB `onButtonPressed` (const $ acceptDialog dlg)
  cancelB `onButtonPressed` (const $ cancelDialog dlg)

  return dlg

onDialogAccept :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogAccept = addHandler dialogAcceptHandlers

onDialogCancel :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogCancel = addHandler dialogCancelHandlers

acceptDialog :: (MonadIO m) => Dialog -> m ()
acceptDialog dlg = fireEvent dlg (return . dialogAcceptHandlers) dlg

cancelDialog :: (MonadIO m) => Dialog -> m ()
cancelDialog dlg = fireEvent dlg (return . dialogCancelHandlers) dlg
