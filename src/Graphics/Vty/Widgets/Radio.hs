module Graphics.Vty.Widgets.Radio
    ( Radio
    , newRadio
    , toggleChecked
    , setRadioUnchecked
    , setRadioChecked
    , onRadioChange
    , radioIsChecked
    )
where

import Data.Maybe
    ( isJust
    , fromJust
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
    ( Attr
    , Key(..)
    , Modifier
    , string
    , region_width
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , (<~)
    , (<~~)
    , getState
    , newWidget
    , updateWidget
    , updateWidgetState_
    )

data Radio = Radio { isChecked :: Bool
                   , normalAttr :: Attr
                   , focusedAttr :: Attr
                   , checkedChar :: Char
                   , radioLabel :: String
                   , radioChangeHandler :: Widget Radio -> Bool -> IO ()
                   }

newRadio :: (MonadIO m) => String -> Attr -> Attr -> m (Widget Radio)
newRadio label normAttr focAttr = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Radio { isChecked = False
                        , normalAttr = normAttr
                        , focusedAttr = focAttr
                        , checkedChar = 'x'
                        , radioLabel = label
                        , radioChangeHandler = \_ _ -> return ()
                        }
        , keyEventHandler = radioKeyEvent
        , draw =
            \this sz mAttr -> do
              f <- focused <~ this
              st <- getState this

              let attr = if isJust mAttr
                         then fromJust mAttr
                         else if f
                              then focusedAttr st
                              else normalAttr st
                  ch = if isChecked st then checkedChar st else ' '

                  s = ['[', ch, ']', ' '] ++ (radioLabel st)

              return $ string attr $ take (fromEnum $ region_width sz) s
        }

radioKeyEvent :: Widget Radio -> Key -> [Modifier] -> IO Bool
radioKeyEvent this (KASCII ' ') [] = toggleChecked this >> return True
radioKeyEvent this KEnter [] = toggleChecked this >> return True
radioKeyEvent _ _ _ = return False

setRadioUnchecked :: (MonadIO m) => Widget Radio -> m ()
setRadioUnchecked wRef = setChecked_ wRef False

setRadioChecked :: (MonadIO m) => Widget Radio -> m ()
setRadioChecked wRef = setChecked_ wRef True

toggleChecked :: (MonadIO m) => Widget Radio -> m ()
toggleChecked wRef = do
  v <- isChecked <~~ wRef
  setChecked_ wRef (not v)

setChecked_ :: (MonadIO m) => Widget Radio -> Bool -> m ()
setChecked_ wRef v = do
  updateWidgetState_ wRef $ \s -> s { isChecked = v }
  notifyChangeHandler wRef

notifyChangeHandler :: (MonadIO m) => Widget Radio -> m ()
notifyChangeHandler wRef = do
  h <- radioChangeHandler <~~ wRef
  v <- radioIsChecked wRef
  liftIO $ h wRef v

radioIsChecked :: (MonadIO m) => Widget Radio -> m Bool
radioIsChecked = (isChecked <~~)

onRadioChange :: Widget Radio -> (Widget Radio -> Bool -> IO ()) -> IO ()
onRadioChange wRef handler = do
  oldHandler <- radioChangeHandler <~~ wRef

  let combinedHandler =
          \w v -> do
            oldHandler w v
            handler w v

  updateWidgetState_ wRef $ \s -> s { radioChangeHandler = combinedHandler }
