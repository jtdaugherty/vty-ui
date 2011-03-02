{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- |This module provides a simple ''dialog'' interface with an ''OK''
-- button and a ''Cancel'' button.  The dialog itself is capable of
-- embedding an arbitrary interface and it exposes ''accept'' and
-- ''cancel'' events which are triggered by the dialog's buttons.
module Graphics.Vty.Widgets.Dialog
    ( Dialog(dialogWidget, setDialogTitle)
    , newDialog
    , onDialogAccept
    , onDialogCancel
    , acceptDialog
    , cancelDialog
    )
where

import Control.Monad.Trans
import Graphics.Vty.Widgets.Centering
import Graphics.Vty.Widgets.Button
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Core

data Dialog = Dialog { dialogWidget :: Widget (Bordered Padded)
                     , setDialogTitle :: String -> IO ()
                     , dialogAcceptHandlers :: Handlers Dialog
                     , dialogCancelHandlers :: Handlers Dialog
                     }

instance HasNormalAttr Dialog where
    setNormalAttribute d a = setNormalAttribute (dialogWidget d) a

-- |Create a new dialog with the specified embedded interface and
-- title.  Returns the dialog itself and the 'FocusGroup' to which its
-- buttons were added, for use in your application.
newDialog :: (MonadIO m, Show a) => Widget a -> String -> m (Dialog, Widget FocusGroup)
newDialog body title = do
  okB <- newButton "OK"
  cancelB <- newButton "Cancel"

  buttonBox <- (return $ buttonWidget okB) <++> (return $ buttonWidget cancelB)
  setBoxSpacing buttonBox 4

  b <- withPadding (padTopBottom 1) =<<
       ((hCentered body) <--> (hCentered buttonBox) >>= withBoxSpacing 1)

  fg <- newFocusGroup
  addToFocusGroup fg $ buttonWidget okB
  addToFocusGroup fg $ buttonWidget cancelB

  b2 <- bordered b >>=
        withBorderedLabel title

  ahs <- newHandlers
  chs <- newHandlers

  let dlg = Dialog { dialogWidget = b2
                   , setDialogTitle = setBorderedLabel b2
                   , dialogAcceptHandlers = ahs
                   , dialogCancelHandlers = chs
                   }

  okB `onButtonPressed` (const $ acceptDialog dlg)
  cancelB `onButtonPressed` (const $ cancelDialog dlg)

  return (dlg, fg)

-- |Register an event handler for the dialog's acceptance event.
onDialogAccept :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogAccept = addHandler (return . dialogAcceptHandlers)

-- |Register an event handler for the dialog's cancellation event.
onDialogCancel :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogCancel = addHandler (return . dialogCancelHandlers)

-- |Programmatically accept the dialog.
acceptDialog :: (MonadIO m) => Dialog -> m ()
acceptDialog dlg = fireEvent dlg (return . dialogAcceptHandlers) dlg

-- |Programmatically cancel the dialog.
cancelDialog :: (MonadIO m) => Dialog -> m ()
cancelDialog dlg = fireEvent dlg (return . dialogCancelHandlers) dlg
