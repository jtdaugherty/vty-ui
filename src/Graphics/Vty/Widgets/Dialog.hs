{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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

data DialogEvent = DialogAccept
                 | DialogCancel
                   deriving (Eq)

data Dialog = Dialog { dialogWidget :: Widget (Bordered Padded)
                     , setDialogTitle :: String -> IO ()
                     , dialogAcceptHandlers :: Handlers Dialog
                     , dialogCancelHandlers :: Handlers Dialog
                     }

instance HasNormalAttr Dialog where
    setNormalAttribute d a = setNormalAttribute (dialogWidget d) a

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

  b2 <- bordered b >>=
        withBorderedLabel title

  setFocusGroup b2 fg

  ahs <- newHandlers
  chs <- newHandlers

  let dlg = Dialog { dialogWidget = b2
                   , setDialogTitle = setBorderedLabel b2
                   , dialogAcceptHandlers = ahs
                   , dialogCancelHandlers = chs
                   }

  okB `onButtonPressed` (const $ acceptDialog dlg)
  cancelB `onButtonPressed` (const $ cancelDialog dlg)

  return dlg

onDialogAccept :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogAccept = addHandler (return . dialogAcceptHandlers)

onDialogCancel :: (MonadIO m) => Dialog -> (Dialog -> IO ()) -> m ()
onDialogCancel = addHandler (return . dialogCancelHandlers)

acceptDialog :: (MonadIO m) => Dialog -> m ()
acceptDialog dlg = fireEvent dlg (return . dialogAcceptHandlers) dlg

cancelDialog :: (MonadIO m) => Dialog -> m ()
cancelDialog dlg = fireEvent dlg (return . dialogCancelHandlers) dlg
