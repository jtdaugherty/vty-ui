{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Focus
    ( FocusGroup
    , newFocusGroup
    , addToFocusGroup
    , addToFocusGroup_
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
import Control.Monad.Reader
    ( ask
    )
import Graphics.Vty
    ( Key(..)
    , DisplayRegion
    , empty_image
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , (<~)
    , (<~~)
    , updateWidgetState_
    , newWidget
    , updateWidget_
    , handleKeyEvent
    , getState
    , growHorizontal
    , growVertical
    , setPhysicalPosition
    , render
    , focus
    , unfocus
    , onLoseFocus
    , onGainFocus
    , onKeyPressed
    )

data FocusEntry = forall a. FocusEntry (Widget a)

data FocusGroup = FocusGroup { entries :: [Widget FocusEntry]
                             , currentEntryNum :: Int
                             }



newFocusEntry :: (MonadIO m) =>
                 Widget a
              -> m (Widget FocusEntry)
newFocusEntry chRef = do
  wRef <- newWidget
  updateWidget_ wRef $ \w ->
      w { state = FocusEntry chRef

        , getGrowHorizontal = do
            (FocusEntry ch) <- ask
            growHorizontal ch

        , getGrowVertical = do
            (FocusEntry ch) <- ask
            growVertical ch

        , draw =
            \this sz mAttr -> do
              (FocusEntry ch) <- getState this
              render ch sz mAttr

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              (FocusEntry ch) <- getState this
              setPhysicalPosition ch pos
        }

  wRef `onLoseFocus` (const $ unfocus chRef)
  wRef `onGainFocus` (const $ focus chRef)
  wRef `onKeyPressed` (\_ k -> handleKeyEvent chRef k)

  return wRef

newFocusGroup :: (MonadIO m) => Widget a -> m (Widget FocusGroup, Widget FocusEntry)
newFocusGroup initialWidget = do
  wRef <- newWidget
  eRef <- newFocusEntry initialWidget
  focus eRef

  updateWidget_ wRef $ \w ->
      w { state = FocusGroup { entries = [eRef]
                             , currentEntryNum = 0
                             }
        , getGrowHorizontal = return False
        , getGrowVertical = return False
        , keyEventHandler =
            \this key mods -> do
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
                       handleKeyEvent e k mods

        -- Should never be rendered.
        , draw = \_ _ _ -> return empty_image
        }

  return (wRef, eRef)

getCursorPosition :: (MonadIO m) => Widget FocusGroup -> m (Maybe DisplayRegion)
getCursorPosition wRef = do
  eRef <- currentEntry wRef
  (FocusEntry w) <- state <~ eRef
  ci <- cursorInfo <~ w
  liftIO (ci w)

currentEntry :: (MonadIO m) => Widget FocusGroup -> m (Widget FocusEntry)
currentEntry wRef = do
  es <- entries <~~ wRef
  i <- currentEntryNum <~~ wRef
  return (es !! i)

addToFocusGroup :: (MonadIO m) => Widget FocusGroup -> Widget a -> m (Widget FocusEntry)
addToFocusGroup cRef wRef = do
  eRef <- newFocusEntry wRef
  updateWidgetState_ cRef $ \s -> s { entries = (entries s) ++ [eRef] }
  return eRef

addToFocusGroup_ :: (MonadIO m) => Widget FocusGroup -> Widget a -> m ()
addToFocusGroup_ cRef wRef = addToFocusGroup cRef wRef >> return ()

focusNext :: (MonadIO m) => Widget FocusGroup -> m ()
focusNext wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  if cur < length (entries st) - 1 then
      setCurrentFocus wRef (cur + 1) else
      setCurrentFocus wRef 0

focusPrevious :: (MonadIO m) => Widget FocusGroup -> m ()
focusPrevious wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  if cur > 0 then
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
         unfocus ((entries st) !! (currentEntryNum st))
         focus ((entries st) !! i)

  updateWidgetState_ cRef $ \s -> s { currentEntryNum = i }
