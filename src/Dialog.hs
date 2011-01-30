{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import System.Exit
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

addHandler :: (MonadIO m) => (w -> IORef [h]) -> w -> h -> m ()
addHandler getRef w handler =
    liftIO $ modifyIORef (getRef w) $ \s -> s ++ [handler]

fireEvent :: (MonadIO m) => w -> (w -> m (IORef [a -> IO ()])) -> a -> m ()
fireEvent w getRef ev = do
  r <- getRef w
  handlers <- liftIO $ readIORef r
  forM_ handlers $ \handler ->
      liftIO $ handler ev

mkHandlers :: (MonadIO m) => m (IORef [a -> IO ()])
mkHandlers = liftIO $ newIORef []

data Button = Button { buttonText :: String
                     , buttonWidget :: Widget Padded
                     , buttonPressedHandlers :: IORef [Button -> IO ()]
                     }

onButtonPressed :: (MonadIO m) => Button -> (Button -> IO ()) -> m ()
onButtonPressed = addHandler buttonPressedHandlers

button :: (MonadIO m) => String -> m Button
button msg = do
  w <- simpleText msg >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  hs <- mkHandlers

  let b = Button msg w hs

  w `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> fireEvent b (return . buttonPressedHandlers) b
          _ -> return ()
        return False

  return b

data DialogEvent = DialogAccept
                 | DialogCancel
                   deriving (Eq)

data Dialog = Dialog { okButton :: Button
                     , cancelButton :: Button
                     , dialogWidget :: Widget (VCentered (HCentered Padded))
                     , setDialogTitle :: String -> IO ()
                     , dialogAcceptHandlers :: IORef [Dialog -> IO ()]
                     , dialogCancelHandlers :: IORef [Dialog -> IO ()]
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
  e `onActivate` \_ -> acceptDialog d

  d `onDialogAccept` const exitSuccess
  d `onDialogCancel` const exitSuccess

  fg `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KEsc -> exitSuccess
        _ -> return False

  runUi (dialogWidget d) $ defaultContext { focusAttr = black `on` yellow }
