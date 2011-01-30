{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import System.Exit
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

data EventHandlers w e = EventHandlers { registeredHandlers :: [(e, IO ())]
                                       -- ^Specific event handlers.
                                       -- Might want an "any" event handler list, too.
                                       }

addEventHandler :: e -> IO () -> EventHandlers w e -> EventHandlers w e
addEventHandler e act eh =
    eh { registeredHandlers = registeredHandlers eh ++ [(e, act)] }

class (Eq e) => EventSource w e where
    getEventHandlers :: w -> IORef (EventHandlers w e)

    onEvent :: (MonadIO m) => e -> w -> IO () -> m ()
    onEvent ev w act =
        liftIO $ modifyIORef (getEventHandlers w) $ addEventHandler ev act

    dispatchEvent :: (MonadIO m) => w -> e -> m ()
    dispatchEvent w ev = do
        let eRef = getEventHandlers w
        eh <- liftIO $ readIORef eRef
        forM_ (registeredHandlers eh) $ \(e', act) ->
            if e' == ev then liftIO act else return ()

data ButtonEvent = ButtonPressed
                   deriving (Eq)

data Button = Button { buttonText :: String
                     , buttonWidget :: Widget Padded
                     , buttonHandlers :: IORef (EventHandlers Button ButtonEvent)
                     }

instance EventSource Button ButtonEvent where
    getEventHandlers = buttonHandlers

onButtonPressed :: (MonadIO m) => Button -> IO () -> m ()
onButtonPressed = onEvent ButtonPressed

button :: (MonadIO m) => String -> m Button
button msg = do
  w <- simpleText msg >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  eRef <- liftIO $ newIORef $ EventHandlers []

  let b = Button msg w eRef

  w `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> dispatchEvent b ButtonPressed
          _ -> return ()
        return False

  return b

data DialogEvent = DialogAccept
                 | DialogCancel
                   deriving (Eq)

instance EventSource Dialog DialogEvent where
    getEventHandlers = dialogHandlers

data Dialog = Dialog { okButton :: Button
                     , cancelButton :: Button
                     , dialogWidget :: Widget (VCentered (HCentered Padded))
                     , setDialogTitle :: String -> IO ()
                     , dialogHandlers :: IORef (EventHandlers Dialog DialogEvent)
                     }

dialog :: (MonadIO m, Show a) => Widget a -> String -> Maybe (Widget FocusGroup)
       -> m Dialog
dialog body title mFg = do
  okB <- button "OK"
  cancelB <- button "Cancel"

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
        withBorderedLabel title >>=
        withNormalAttribute (white `on` blue)

  c <- centered =<< withPadding (padLeftRight 10) b2

  setFocusGroup c fg
  eRef <- liftIO $ newIORef $ EventHandlers []

  let dlg = Dialog { okButton = okB
                   , cancelButton = cancelB
                   , dialogWidget = c
                   , setDialogTitle = setBorderedLabel b2
                   , dialogHandlers = eRef
                   }

  okB `onButtonPressed` dispatchEvent dlg DialogAccept
  cancelB `onButtonPressed` dispatchEvent dlg DialogCancel

  return dlg

onDialogAccept :: (MonadIO m) => Dialog -> IO () -> m ()
onDialogAccept = onEvent DialogAccept

onDialogCancel :: (MonadIO m) => Dialog -> IO () -> m ()
onDialogCancel = onEvent DialogCancel

main :: IO ()
main = do
  e <- editWidget
  fg <- newFocusGroup
  addToFocusGroup fg e

  u <- simpleText "Enter some text and press enter." <--> return e
       >>= withBoxSpacing 1

  pe <- padded u (padLeftRight 2)
  d <- dialog pe "<enter text>" (Just fg)

  let updateTitle = setDialogTitle d =<< getEditText e

  e `onChange` \_ _ -> updateTitle

  d `onDialogAccept` exitSuccess
  d `onDialogCancel` exitSuccess

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi (dialogWidget d) $ defaultContext { focusAttr = black `on` yellow }
