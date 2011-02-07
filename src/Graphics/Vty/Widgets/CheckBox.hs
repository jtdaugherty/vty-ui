{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Vty.Widgets.CheckBox
    ( CheckBox
    , RadioGroup
    , newCheckbox
    , newRadioGroup
    , onRadioChange
    , addToRadioGroup
    , getCurrentRadio
    , toggleChecked
    , setCheckboxUnchecked
    , setCheckboxChecked
    , setCheckedChar
    , onCheckboxChange
    , checkboxIsChecked
    , getCheckboxLabel
    )
where

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data RadioGroupData = RadioGroupData { currentlySelected :: Maybe (Widget CheckBox)
                                     , changeHandlers :: Handlers (Widget CheckBox)
                                     }

type RadioGroup = IORef RadioGroupData

newRadioGroup :: (MonadIO m) => m RadioGroup
newRadioGroup = do
  hs <- mkHandlers
  liftIO $ newIORef $ RadioGroupData Nothing hs

onRadioChange :: (MonadIO m) => RadioGroup -> (Widget CheckBox -> IO ())
              -> m ()
onRadioChange rg act = do
  rd <- liftIO $ readIORef rg
  addHandler (return . changeHandlers) rd act

getCurrentRadio :: (MonadIO m) => RadioGroup -> m (Maybe (Widget CheckBox))
getCurrentRadio = (currentlySelected <~)

setCheckedChar :: (MonadIO m) => Widget CheckBox -> Char -> m ()
setCheckedChar wRef ch =
    updateWidgetState wRef $ \s -> s { checkedChar = ch
                                      }

addToRadioGroup :: (MonadIO m) => RadioGroup -> Widget CheckBox -> m ()
addToRadioGroup rg wRef = do
  updateWidgetState wRef $ \s -> s { radioGroup = Just rg
                                   }
  setCheckedChar wRef '*'
  setCheckboxUnchecked wRef

  wRef `onCheckboxChange` \v ->
      when v $ do
        rd <- readIORef rg
        fireEvent rd (return . changeHandlers) wRef

radioGroupSetCurrent :: (MonadIO m) => Widget CheckBox -> m ()
radioGroupSetCurrent wRef = do
  mRg <- radioGroup <~~ wRef

  when (isJust mRg) $
       do
         let Just rg = mRg
         rgData <- liftIO $ readIORef rg

         -- If the radio group has a currently-selected checkbox,
         -- uncheck it (but only if it's a different widget: it could
         -- be the only one in this group!)
         when ((isJust $ currentlySelected rgData) &&
               (currentlySelected rgData /= Just wRef)) $
              setChecked__ (fromJust $ currentlySelected rgData) False

         liftIO $ writeIORef rg $ rgData { currentlySelected = Just wRef }
         setChecked__ wRef True

data CheckBox = CheckBox { isChecked :: Bool
                         , checkedChar :: Char
                         , checkboxLabel :: String
                         , checkboxChangeHandlers :: Handlers Bool
                         , radioGroup :: Maybe RadioGroup
                         }

instance Show CheckBox where
    show cb = concat [ "CheckBox { "
                     , "isChecked = ", show $ isChecked cb
                     , ", checkedChar = ", show $ checkedChar cb
                     , ", checkboxLabel = ", show $ checkboxLabel cb
                     , " }"
                     ]

newCheckbox :: (MonadIO m) => String -> m (Widget CheckBox)
newCheckbox label = do
  wRef <- newWidget
  cchs <- mkHandlers
  updateWidget wRef $ \w ->
      w { state = CheckBox { isChecked = False
                           , checkedChar = 'x'
                           , checkboxLabel = label
                           , checkboxChangeHandlers = cchs
                           , radioGroup = Nothing
                           }
        , cursorInfo =
            \this -> do
              pos <- getCurrentPosition this
              return $ Just (pos `plusWidth` 1)

        , keyEventHandler = radioKeyEvent
        , render_ =
            \this sz ctx -> do
              f <- focused <~ this
              st <- getState this

              let attr = mergeAttrs [ overrideAttr ctx
                                    , normalAttr ctx
                                    ]

                  fAttr = if f
                          then focusAttr ctx
                          else attr

                  ch = if isChecked st then checkedChar st else ' '

                  s = ['[', ch, ']', ' '] ++ (checkboxLabel st)

              return $ string fAttr $ take (fromEnum $ region_width sz) s
        }
  return wRef

getCheckboxLabel :: (MonadIO m) => Widget CheckBox -> m String
getCheckboxLabel = (checkboxLabel <~~)

radioKeyEvent :: Widget CheckBox -> Key -> [Modifier] -> IO Bool
radioKeyEvent this (KASCII ' ') [] = toggleChecked this >> return True
radioKeyEvent this KEnter [] = toggleChecked this >> return True
radioKeyEvent _ _ _ = return False

setCheckboxUnchecked :: (MonadIO m) => Widget CheckBox -> m ()
setCheckboxUnchecked wRef = setChecked_ wRef False

setCheckboxChecked :: (MonadIO m) => Widget CheckBox -> m ()
setCheckboxChecked wRef = setChecked_ wRef True

toggleChecked :: (MonadIO m) => Widget CheckBox -> m ()
toggleChecked wRef = do
  v <- isChecked <~~ wRef
  setChecked_ wRef (not v)

setChecked_ :: (MonadIO m) => Widget CheckBox -> Bool -> m ()
setChecked_ wRef v = do
  mRg <- radioGroup <~~ wRef

  case mRg of
    Nothing -> setChecked__ wRef v
    Just _ -> case v of
                True -> radioGroupSetCurrent wRef
                -- unchecking a radio button is not permitted except
                -- by the internal API (setChecked__)
                False -> return ()

setChecked__ :: (MonadIO m) => Widget CheckBox -> Bool -> m ()
setChecked__ wRef v = do
  oldVal <- isChecked <~~ wRef

  when (oldVal /= v) $
       do
         updateWidgetState wRef $ \s -> s { isChecked = v }
         notifyChangeHandlers wRef

notifyChangeHandlers :: (MonadIO m) => Widget CheckBox -> m ()
notifyChangeHandlers wRef = do
  v <- checkboxIsChecked wRef
  fireEvent wRef (checkboxChangeHandlers <~~) v

checkboxIsChecked :: (MonadIO m) => Widget CheckBox -> m Bool
checkboxIsChecked = (isChecked <~~)

onCheckboxChange :: (MonadIO m) => Widget CheckBox -> (Bool -> IO ()) -> m ()
onCheckboxChange = addHandler (checkboxChangeHandlers <~~)
