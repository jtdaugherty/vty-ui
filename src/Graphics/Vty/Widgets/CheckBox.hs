{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable,
  OverloadedStrings #-}
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
import qualified Data.Text as T
import Control.Monad
import Control.Exception
import Data.Typeable
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data RadioGroupData = RadioGroupData { currentlySelected :: Maybe (Widget (CheckBox Bool))
                                     , changeHandlers :: Handlers (Widget (CheckBox Bool))
                                     }

type RadioGroup = IORef RadioGroupData

-- |Create a new radio button group.  This is used to guarantee
-- exclusivity among the check boxes in the group so that they behave
-- like radio buttons.
newRadioGroup :: IO RadioGroup
newRadioGroup = do
  hs <- newHandlers
  newIORef $ RadioGroupData Nothing hs

-- |Register a handler to be notified when the currently-selected
-- check box in a radio group changes.
onRadioChange :: RadioGroup -> (Widget (CheckBox Bool) -> IO ())
              -> IO ()
onRadioChange rg act = do
  rd <- readIORef rg
  addHandler (return . changeHandlers) rd act

-- |Get the currently-selected checkbox in a radio group, if any.
getCurrentRadio :: RadioGroup -> IO (Maybe (Widget (CheckBox Bool)))
getCurrentRadio = (currentlySelected <~)

-- |Add a check box to a radio group.  The check box's apperance will
-- be changed so that it resembles a radio button.
addToRadioGroup :: RadioGroup -> Widget (CheckBox Bool) -> IO ()
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

        rgData <- readIORef rg

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
                           , checkboxLabel :: T.Text
                           , checkboxChangeHandlers :: Handlers a
                           , checkboxFrozen :: Bool
                           , innerTextWidget :: Widget FormattedText
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
newCheckbox :: T.Text -> IO (Widget (CheckBox Bool))
newCheckbox label = newMultiStateCheckbox label [(False, ' '), (True, 'x')]

-- |Create a new multi-state checkbox.
newMultiStateCheckbox :: (Eq a) =>
                         T.Text -- ^The checkbox label.
                      -> [(a, Char)] -- ^The list of valid states that
                                     -- the checkbox can be in, along
                                     -- with the visual representation
                                     -- ('Char') for each state.
                      -> IO (Widget (CheckBox a))
newMultiStateCheckbox _ [] = throw EmptyCheckboxStates
newMultiStateCheckbox label states = do
  cchs <- newHandlers
  t <- plainText ""
  let initSt = CheckBox { checkboxLabel = label
                        , checkboxChangeHandlers = cchs
                        , leftBracketChar = '['
                        , rightBracketChar = ']'
                        , checkboxStates = states
                        , currentState = fst $ states !! 0
                        , checkboxFrozen = False
                        , innerTextWidget = t
                        }

  wRef <- newWidget initSt $ \w ->
      w { getCursorPosition_ =
            \this -> do
              pos <- getCurrentPosition this
              ch <- leftBracketChar <~~ this
              return $ Just (pos `plusWidth` (toEnum $ fromEnum $ chWidth ch))

        , keyEventHandler = radioKeyEvent
        , render_ =
            \this sz ctx -> do
              st <- getState this
              tw <- innerTextWidget <~~ this

              let v = currentState st
                  ch = fromJust $ lookup v (checkboxStates st)

                  s = T.concat [ T.pack [ leftBracketChar st
                                        , ch
                                        , rightBracketChar st
                                        , ' '
                                        ]
                               , checkboxLabel st
                               ]

              setText tw s
              render tw sz ctx
        }

  wRef `relayFocusEvents` t
  setTextAppearFocused t True

  return wRef

modifyElem :: [a] -> Int -> (a -> a) -> [a]
modifyElem as i f = concat [ take i as
                           , [f $ as !! i]
                           , drop (i + 1) as
                           ]

-- |Set the visual representation for a state in a checkbox.  May
-- throw 'BadStateArgument'.
setStateChar :: (Eq a) => Widget (CheckBox a) -> a -> Char -> IO ()
setStateChar wRef v ch = do
  states <- checkboxStates <~~ wRef

  let mIdx = findIndex ((== v) . fst) states
  when (isNothing mIdx) $ throw BadStateArgument

  let Just i = mIdx
      newStates = modifyElem states i (\(k, _) -> (k, ch))

  updateWidgetState wRef $ \s -> s { checkboxStates = newStates }

-- |Set the checkbox's bracketing characters for the left and right
-- brackets around the state character.
setBracketChars :: Widget (CheckBox a) -> Char -> Char -> IO ()
setBracketChars wRef chL chR =
    updateWidgetState wRef $ \s -> s { leftBracketChar = chL
                                     , rightBracketChar = chR
                                     }

-- |Get a checkbox's text label.
getCheckboxLabel :: Widget (CheckBox a) -> IO T.Text
getCheckboxLabel = (checkboxLabel <~~)

radioKeyEvent :: (Eq a) => Widget (CheckBox a) -> Key -> [Modifier] -> IO Bool
radioKeyEvent this (KChar ' ') [] = cycleCheckbox this >> return True
radioKeyEvent this KEnter [] = cycleCheckbox this >> return True
radioKeyEvent _ _ _ = return False

-- |Set the state of a checkbox.  May throw 'BadCheckboxState'.
setCheckboxState :: (Eq a) => Widget (CheckBox a) -> a -> IO ()
setCheckboxState = setChecked_

-- |Set a binary checkbox to unchecked.
setCheckboxUnchecked :: Widget (CheckBox Bool) -> IO ()
setCheckboxUnchecked wRef = setCheckboxState wRef False

-- |Set a binary checkbox to checked.
setCheckboxChecked :: Widget (CheckBox Bool) -> IO ()
setCheckboxChecked wRef = setCheckboxState wRef True

-- |Toggle a binary checkbox.
toggleCheckbox :: Widget (CheckBox Bool) -> IO ()
toggleCheckbox wRef = do
  v <- currentState <~~ wRef
  setCheckboxState wRef (not v)

-- |Get a checkbox's current state value.
getCheckboxState :: Widget (CheckBox a) -> IO a
getCheckboxState = (currentState <~~)

-- |Cycle a checkbox's state to the next value in its state list.
cycleCheckbox :: (Eq a) => Widget (CheckBox a) -> IO ()
cycleCheckbox wRef = do
  v <- currentState <~~ wRef
  states <- checkboxStates <~~ wRef
  let Just curI = findIndex ((== v) . fst) states
      nextI = (curI + 1) `mod` length states
  setChecked_ wRef $ fst $ states !! nextI

setChecked_ :: (Eq a) => Widget (CheckBox a) -> a -> IO ()
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

notifyChangeHandlers :: Widget (CheckBox a) -> IO ()
notifyChangeHandlers wRef = do
  v <- currentState <~~ wRef
  fireEvent wRef (checkboxChangeHandlers <~~) v

-- |Register a handler for a checkbox state change.  The handler will
-- be passed the new state value.
onCheckboxChange :: Widget (CheckBox a) -> (a -> IO ()) -> IO ()
onCheckboxChange = addHandler (checkboxChangeHandlers <~~)

thaw :: Widget (CheckBox a) -> IO ()
thaw wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = False }

freeze :: Widget (CheckBox a) -> IO ()
freeze wRef = updateWidgetState wRef $ \s -> s { checkboxFrozen = True }
