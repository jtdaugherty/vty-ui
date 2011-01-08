{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Focus
    ( FocusGroup
    , newFocusGroup
    , addToFocusGroup
    , focusNext
    , focusPrevious
    , setCurrentFocus
    , getCursorPosition
    )
where

import Control.Monad
    ( when
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
    ( Key(..)
    , DisplayRegion
    , empty_image
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget
    , WidgetImpl(..)
    , (<~)
    , (<~~)
    , updateWidgetState_
    , newWidget
    , updateWidget
    , handleKeyEvent
    , getState
    )

data FocusEntry = forall a. FocusEntry (Widget a)

data FocusGroup = FocusGroup { entries :: [FocusEntry]
                             , currentEntryNum :: Int
                             }

newFocusGroup :: (MonadIO m) => m (Widget FocusGroup)
newFocusGroup = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = FocusGroup { entries = []
                             , currentEntryNum = -1
                             }
        , getGrowHorizontal = return False
        , getGrowVertical = return False
        , keyEventHandler =
            \this key -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return False
                i -> do
                  case key of
                    (KASCII '\t') -> do
                             focusNext this
                             return True
                    k -> do
                       let e = entries st !! i
                       entryHandleKeyEvent e k

        -- Should never be rendered.
        , draw = \_ _ _ -> return empty_image
        }

getCursorPosition :: (MonadIO m) => Widget FocusGroup -> m (Maybe DisplayRegion)
getCursorPosition wRef = do
  (FocusEntry w) <- currentEntry wRef
  ci <- cursorInfo <~ w
  liftIO (ci w)

currentEntry :: (MonadIO m) => Widget FocusGroup -> m FocusEntry
currentEntry wRef = do
  es <- entries <~~ wRef
  i <- currentEntryNum <~~ wRef
  if i >= 0 && i < (length es) then
      return (es !! i) else
      error "Cannot get current entry of an empty focus group"

entryHandleKeyEvent :: (MonadIO m) => FocusEntry -> Key -> m Bool
entryHandleKeyEvent (FocusEntry w) k = handleKeyEvent w k

addToFocusGroup :: (MonadIO m) => Widget FocusGroup -> Widget a -> m ()
addToFocusGroup cRef wRef =
    updateWidgetState_ cRef $ \st ->
        st { entries = (entries st) ++ [FocusEntry wRef]
           , currentEntryNum = if currentEntryNum st == -1
                               then 0
                               else currentEntryNum st
           }

focusNext :: (MonadIO m) => Widget FocusGroup -> m ()
focusNext wRef = do
  st <- getState wRef
  case currentEntryNum st of
    (-1) -> return ()
    cur -> if cur < length (entries st) - 1 then
               setCurrentFocus wRef (cur + 1) else
               setCurrentFocus wRef 0

focusPrevious :: (MonadIO m) => Widget FocusGroup -> m ()
focusPrevious wRef = do
  st <- getState wRef
  case currentEntryNum st of
    (-1) -> return ()
    cur -> if cur > 0 then
               setCurrentFocus wRef (cur - 1) else
               setCurrentFocus wRef (length (entries st) - 1)

setCurrentFocus :: (MonadIO m) => Widget FocusGroup -> Int -> m ()
setCurrentFocus cRef i = do
  st <- state <~ cRef
  when (i >= length (entries st) || i < 0) $
       error $ "collection index " ++ (show i) ++
                 " bad; size is " ++ (show $ length $ entries st)

  -- If new entry number is different from existing one, invoke focus
  -- handlers.
  when (currentEntryNum st /= i) $
       do
         entryLoseFocus ((entries st) !! (currentEntryNum st))
         entryGainFocus ((entries st) !! i)

  updateWidgetState_ cRef $ \s -> s { currentEntryNum = i }

entryLoseFocus :: (MonadIO m) => FocusEntry -> m ()
entryLoseFocus (FocusEntry w) = do
  act <- loseFocus <~ w
  liftIO $ act w

entryGainFocus :: (MonadIO m) => FocusEntry -> m ()
entryGainFocus (FocusEntry w) = do
  act <- gainFocus <~ w
  liftIO $ act w
