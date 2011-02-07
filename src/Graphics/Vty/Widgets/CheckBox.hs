{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Graphics.Vty.Widgets.CheckBox
    ( CheckBox
    , RadioGroup

    -- * Traditional binary-mode checkboxes
    , newCheckbox
    , setCheckboxUnchecked
    , setCheckboxChecked
    , toggleCheckbox

    -- * Event handler registration
    , onCheckboxChange

    -- * Generalized checkbox functions
    , newMultiStateCheckbox
    , setCheckboxState
    , cycleCheckbox
    , setStateChar
    , setBracketChars
    , getCheckboxLabel
    , getCheckboxState

    -- * Radio groups
    , newRadioGroup
    , onRadioChange
    , addToRadioGroup
    , getCurrentRadio
    )
where

import Data.IORef
import Data.List ( findIndex )
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Typeable
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data RadioGroupData = RadioGroupData { currentlySelected :: Maybe (Widget (CheckBox Bool))
                                     , changeHandlers :: Handlers (Widget (CheckBox Bool))
                                     }

type RadioGroup = IORef RadioGroupData

newRadioGroup :: (MonadIO m) => m RadioGroup
newRadioGroup = do
  hs <- mkHandlers
  liftIO $ newIORef $ RadioGroupData Nothing hs

onRadioChange :: (MonadIO m) => RadioGroup -> (Widget (CheckBox Bool) -> IO ())
              -> m ()
onRadioChange rg act = do
  rd <- liftIO $ readIORef rg
  addHandler (return . changeHandlers) rd act

getCurrentRadio :: (MonadIO m) => RadioGroup -> m (Maybe (Widget (CheckBox Bool)))
getCurrentRadio = (currentlySelected <~)

addToRadioGroup :: (MonadIO m) => RadioGroup -> Widget (CheckBox Bool) -> m ()
addToRadioGroup rg wRef = do
  setStateChar wRef True '*'
  setBracketChars wRef '(' ')'
  setCheckboxUnchecked wRef

  wRef `onCheckboxChange` \v ->
      when v $ do
        rd <- readIORef rg
        fireEvent rd (return . changeHandlers) wRef

  wRef `onCheckboxChange` \v ->
      when v $ do
        -- Uncheck the old currently-selected checkbox in the radio
        -- group, if any, before updating the radiogroup state.

        rgData <- liftIO $ readIORef rg

        -- If the radio group has a currently-selected checkbox,
        -- uncheck it (but only if it's a different widget: it could
        -- be the only one in this group!)
        when ((isJust $ currentlySelected rgData) &&
              (currentlySelected rgData /= Just wRef)) $ do
                            let cur = fromJust $ currentlySelected rgData
                            thaw cur
                            setChecked_ cur False

        freeze wRef

        writeIORef rg $ rgData { currentlySelected = Just wRef }

data CheckBoxError = EmptyCheckboxStates
                   | BadCheckboxState
                   | BadStateArgument
                     deriving (Show, Typeable)

instance Exception CheckBoxError where

data CheckBox a = CheckBox { leftBracketChar :: Char
                           , rightBracketChar :: Char
                           , checkboxStates :: [(a, Char)]
                           , currentState :: a
                           , checkboxLabel :: String
                           , checkboxChangeHandlers :: Handlers a
                           , checkboxFrozen :: Bool
                           }

instance Show a => Show (CheckBox a) where
    show cb = concat [ "CheckBox { "
                     , "  checkboxLabel = ", show $ checkboxLabel cb
                     , ", checkboxStates = ", show $ checkboxStates cb
                     , ", currentState = ", show $ currentState cb
                     , ", checkboxFrozen = ", show $ checkboxFrozen cb
                     , " }"
                     ]

newCheckbox :: (MonadIO m) => String -> m (Widget (CheckBox Bool))
newCheckbox label = newMultiStateCheckbox label [(False, ' '), (True, 'x')]

newMultiStateCheckbox :: (Eq a, MonadIO m) => String -> [(a, Char)] -> m (Widget (CheckBox a))
newMultiStateCheckbox _ [] = throw EmptyCheckboxStates
newMultiStateCheckbox label states = do
  wRef <- newWidget
  cchs <- mkHandlers
  updateWidget wRef $ \w ->
      w { state = CheckBox { checkboxLabel = label
                           , checkboxChangeHandlers = cchs
                           , leftBracketChar = '['
                           , rightBracketChar = ']'
                           , checkboxStates = states
                           , currentState = fst $ states !! 0
                           , checkboxFrozen = False
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

                  v = currentState st
                  ch = fromJust $ lookup v (checkboxStates st)

                  s = [leftBracketChar st, ch, rightBracketChar st, ' '] ++
                      (checkboxLabel st)

              return $ string fAttr $ take (fromEnum $ region_width sz) s
        }
  return wRef

modifyElem :: [a] -> Int -> (a -> a) -> [a]
modifyElem as i f = concat [ take i as
                           , [f $ as !! i]
                           , drop (i + 1) as
                           ]

setStateChar :: (Eq a, MonadIO m) => Widget (CheckBox a) -> a -> Char -> m ()
setStateChar wRef v ch = do
  states <- checkboxStates <~~ wRef

  let mIdx = findIndex ((== v) . fst) states
  when (isNothing mIdx) $ throw BadStateArgument

  let Just i = mIdx
      newStates = modifyElem states i (\(k, _) -> (k, ch))

  updateWidgetState wRef $ \s -> s { checkboxStates = newStates }

setBracketChars :: (MonadIO m) => Widget (CheckBox a) -> Char -> Char -> m ()
setBracketChars wRef chL chR =
    updateWidgetState wRef $ \s -> s { leftBracketChar = chL
                                     , rightBracketChar = chR
                                     }

getCheckboxLabel :: (MonadIO m) => Widget (CheckBox a) -> m String
getCheckboxLabel = (checkboxLabel <~~)

radioKeyEvent :: (Eq a) => Widget (CheckBox a) -> Key -> [Modifier] -> IO Bool
radioKeyEvent this (KASCII ' ') [] = cycleCheckbox this >> return True
radioKeyEvent this KEnter [] = cycleCheckbox this >> return True
radioKeyEvent _ _ _ = return False

setCheckboxState :: (Eq a, MonadIO m) => Widget (CheckBox a) -> a -> m ()
setCheckboxState = setChecked_

setCheckboxUnchecked :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
setCheckboxUnchecked wRef = setCheckboxState wRef False

setCheckboxChecked :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
setCheckboxChecked wRef = setCheckboxState wRef True

toggleCheckbox :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
toggleCheckbox wRef = do
  v <- currentState <~~ wRef
  setCheckboxState wRef (not v)

getCheckboxState :: (MonadIO m) => Widget (CheckBox a) -> m a
getCheckboxState = (currentState <~~)

cycleCheckbox :: (Eq a, MonadIO m) => Widget (CheckBox a) -> m ()
cycleCheckbox wRef = do
  v <- currentState <~~ wRef
  states <- checkboxStates <~~ wRef
  let Just curI = findIndex ((== v) . fst) states
      nextI = (curI + 1) `mod` length states
  setChecked_ wRef $ fst $ states !! nextI

setChecked_ :: (Eq a, MonadIO m) => Widget (CheckBox a) -> a -> m ()
setChecked_ wRef v = do
  f <- checkboxFrozen <~~ wRef

  when (not f) $ do
    oldV <- currentState <~~ wRef
    states <- checkboxStates <~~ wRef

    when (not $ v `elem` (map fst states)) $
         throw BadCheckboxState

    when (oldV /= v) $
         do
           updateWidgetState wRef $ \s -> s { currentState = v }
           notifyChangeHandlers wRef

notifyChangeHandlers :: (MonadIO m) => Widget (CheckBox a) -> m ()
notifyChangeHandlers wRef = do
  v <- currentState <~~ wRef
  fireEvent wRef (checkboxChangeHandlers <~~) v

onCheckboxChange :: (MonadIO m) => Widget (CheckBox a) -> (a -> IO ()) -> m ()
onCheckboxChange = addHandler (checkboxChangeHandlers <~~)

thaw :: (MonadIO m) => Widget (CheckBox a) -> m ()
thaw wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = False }

freeze :: (MonadIO m) => Widget (CheckBox a) -> m ()
freeze wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = True }