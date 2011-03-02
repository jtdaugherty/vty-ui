{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
-- |This module provides ''check box'' widgets and ''radio button''
-- widgets.  In addition, this module provides a generalized
-- ''multi-state'' check box type which allows you to set multiple
-- states in the checkbox, each with its own character
-- representation.
--
-- All of these types of widgets are toggled with the Spacebar and
-- Enter keys.
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

-- |Create a new radio button group.  This is used to guarantee
-- exclusivity among the check boxes in the group so that they behave
-- like radio buttons.
newRadioGroup :: (MonadIO m) => m RadioGroup
newRadioGroup = do
  hs <- newHandlers
  liftIO $ newIORef $ RadioGroupData Nothing hs

-- |Register a handler to be notified when the currently-selected
-- check box in a radio group changes.
onRadioChange :: (MonadIO m) => RadioGroup -> (Widget (CheckBox Bool) -> IO ())
              -> m ()
onRadioChange rg act = do
  rd <- liftIO $ readIORef rg
  addHandler (return . changeHandlers) rd act

-- |Get the currently-selected checkbox in a radio group, if any.
getCurrentRadio :: (MonadIO m) => RadioGroup -> m (Maybe (Widget (CheckBox Bool)))
getCurrentRadio = (currentlySelected <~)

-- |Add a check box to a radio group.  The check box's apperance will
-- be changed so that it resembles a radio button.
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
                   -- ^Indicates that an empty state list was used to
                   -- create a multi-state checkbox.
                   | BadCheckboxState
                   -- ^Indicates that a checkbox state value is not a
                   -- valid state value in the checkbox's state
                   -- mapping.
                   | BadStateArgument
                     -- ^Indicates that a state argument used for a
                     -- checkbox state transition is not a valid state
                     -- for the checkbox.
                     deriving (Show, Typeable)

instance Exception CheckBoxError

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

-- |Create a new checkbox with the specified text label.
newCheckbox :: (MonadIO m) => String -> m (Widget (CheckBox Bool))
newCheckbox label = newMultiStateCheckbox label [(False, ' '), (True, 'x')]

-- |Create a new multi-state checkbox.
newMultiStateCheckbox :: (Eq a, MonadIO m) =>
                         String -- ^The checkbox label.
                      -> [(a, Char)] -- ^The list of valid states that
                                     -- the checkbox can be in, along
                                     -- with the visual representation
                                     -- ('Char') for each state.
                      -> m (Widget (CheckBox a))
newMultiStateCheckbox _ [] = throw EmptyCheckboxStates
newMultiStateCheckbox label states = do
  cchs <- newHandlers
  wRef <- newWidget $ \w ->
      w { state = CheckBox { checkboxLabel = label
                           , checkboxChangeHandlers = cchs
                           , leftBracketChar = '['
                           , rightBracketChar = ']'
                           , checkboxStates = states
                           , currentState = fst $ states !! 0
                           , checkboxFrozen = False
                           }
        , getCursorPosition_ =
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

-- |Set the visual representation for a state in a checkbox.  May
-- throw 'BadStateArgument'.
setStateChar :: (Eq a, MonadIO m) => Widget (CheckBox a) -> a -> Char -> m ()
setStateChar wRef v ch = do
  states <- checkboxStates <~~ wRef

  let mIdx = findIndex ((== v) . fst) states
  when (isNothing mIdx) $ throw BadStateArgument

  let Just i = mIdx
      newStates = modifyElem states i (\(k, _) -> (k, ch))

  updateWidgetState wRef $ \s -> s { checkboxStates = newStates }

-- |Set the checkbox's bracketing characters for the left and right
-- brackets around the state character.
setBracketChars :: (MonadIO m) => Widget (CheckBox a) -> Char -> Char -> m ()
setBracketChars wRef chL chR =
    updateWidgetState wRef $ \s -> s { leftBracketChar = chL
                                     , rightBracketChar = chR
                                     }

-- |Get a checkbox's text label.
getCheckboxLabel :: (MonadIO m) => Widget (CheckBox a) -> m String
getCheckboxLabel = (checkboxLabel <~~)

radioKeyEvent :: (Eq a) => Widget (CheckBox a) -> Key -> [Modifier] -> IO Bool
radioKeyEvent this (KASCII ' ') [] = cycleCheckbox this >> return True
radioKeyEvent this KEnter [] = cycleCheckbox this >> return True
radioKeyEvent _ _ _ = return False

-- |Set the state of a checkbox.  May throw 'BadCheckboxState'.
setCheckboxState :: (Eq a, MonadIO m) => Widget (CheckBox a) -> a -> m ()
setCheckboxState = setChecked_

-- |Set a binary checkbox to unchecked.
setCheckboxUnchecked :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
setCheckboxUnchecked wRef = setCheckboxState wRef False

-- |Set a binary checkbox to checked.
setCheckboxChecked :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
setCheckboxChecked wRef = setCheckboxState wRef True

-- |Toggle a binary checkbox.
toggleCheckbox :: (MonadIO m) => Widget (CheckBox Bool) -> m ()
toggleCheckbox wRef = do
  v <- currentState <~~ wRef
  setCheckboxState wRef (not v)

-- |Get a checkbox's current state value.
getCheckboxState :: (MonadIO m) => Widget (CheckBox a) -> m a
getCheckboxState = (currentState <~~)

-- |Cycle a checkbox's state to the next value in its state list.
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

-- |Register a handler for a checkbox state change.  The handler will
-- be passed the new state value.
onCheckboxChange :: (MonadIO m) => Widget (CheckBox a) -> (a -> IO ()) -> m ()
onCheckboxChange = addHandler (checkboxChangeHandlers <~~)

thaw :: (MonadIO m) => Widget (CheckBox a) -> m ()
thaw wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = False }

freeze :: (MonadIO m) => Widget (CheckBox a) -> m ()
freeze wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = True }