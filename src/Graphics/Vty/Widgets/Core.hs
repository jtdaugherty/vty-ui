{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances #-}
-- |This module is the core of this library; it provides
-- infrastructure for creating new types of widgets and extending
-- their functionality.  This module provides various bits of
-- infrastructure, including:
--
-- * modeling user interface widgets
--
-- * managing changes in focus between widgets
--
-- * managing widget state
--
-- This module does not provide any concrete widget types.  For
-- in-depth discussion on this module's API and widget implementation
-- in particular, see the Vty-ui User's Manual.
module Graphics.Vty.Widgets.Core
    (
    -- ** Widget Infrastructure
      WidgetImpl(..)
    , Widget
    , getNormalAttr
    , defaultContext
    , updateWidget
    , updateWidgetState
    , newWidget
    , getState
    , getCurrentSize
    , setCurrentPosition
    , getCurrentPosition
    , growVertical
    , growHorizontal
    , getCursorPosition
    , showWidget
    , (<~)
    , (<~~)

    -- ** Rendering
    , RenderContext(..)
    , RenderError(..)
    , render
    , renderAndPosition

    -- ** Miscellaneous
    , HasNormalAttr(..)
    , HasFocusAttr(..)
    , withNormalAttribute
    , withFocusAttribute

    -- ** Events
    , handleKeyEvent
    , onKeyPressed
    , onGainFocus
    , onLoseFocus
    , relayKeyEvents
    , relayFocusEvents

    -- ** Focus management
    , FocusGroup
    , FocusGroupError(..)
    , newFocusGroup
    , mergeFocusGroups
    , resetFocusGroup
    , addToFocusGroup
    , focusNext
    , focusPrevious
    , setFocusGroupNextKey
    , setFocusGroupPrevKey
    , focus
    , unfocus
    )
where

import Data.Typeable
import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Graphics.Vty
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Skins
import Graphics.Vty.Widgets.Events

class HasNormalAttr w where
    setNormalAttribute :: (MonadIO m) => w -> Attr -> m ()

class HasFocusAttr w where
    setFocusAttribute :: (MonadIO m) => w -> Attr -> m ()

instance HasNormalAttr (Widget a) where
    setNormalAttribute wRef a =
        updateWidget wRef $ \w -> w { normalAttribute = mergeAttr a (normalAttribute w) }

instance HasFocusAttr (Widget a) where
    setFocusAttribute wRef a =
        updateWidget wRef $ \w -> w { focusAttribute = mergeAttr a (focusAttribute w) }

withNormalAttribute :: (HasNormalAttr w, MonadIO m) => Attr -> w -> m w
withNormalAttribute att w = do
  setNormalAttribute w att
  return w

withFocusAttribute :: (HasFocusAttr w, MonadIO m) => Attr -> w -> m w
withFocusAttribute att w = do
  setFocusAttribute w att
  return w

data RenderError = ImageTooBig String DisplayRegion DisplayRegion
                   deriving (Show, Typeable)

instance Exception RenderError

data RenderContext = RenderContext { normalAttr :: Attr
                                   , focusAttr :: Attr
                                   , overrideAttr :: Attr
                                   , skin :: Skin
                                   }

getNormalAttr :: RenderContext -> Attr
getNormalAttr ctx = mergeAttrs [ overrideAttr ctx, normalAttr ctx ]

defaultContext :: RenderContext
defaultContext = RenderContext def_attr def_attr def_attr unicodeSkin

data WidgetImpl a = WidgetImpl {
      state :: a
    , render_ :: Widget a -> DisplayRegion -> RenderContext -> IO Image
    , growHorizontal_ :: a -> IO Bool
    , growVertical_ :: a -> IO Bool
    , currentSize :: DisplayRegion
    , currentPosition :: DisplayRegion
    , normalAttribute :: Attr
    , focusAttribute :: Attr
    , setCurrentPosition_ :: Widget a -> DisplayRegion -> IO ()
    , keyEventHandler :: Widget a -> Key -> [Modifier] -> IO Bool
    , gainFocusHandlers :: Handlers (Widget a)
    , loseFocusHandlers :: Handlers (Widget a)
    , focused :: Bool
    , getCursorPosition_ :: Widget a -> IO (Maybe DisplayRegion)
    }

type Widget a = IORef (WidgetImpl a)

showWidget :: (Functor m, MonadIO m, Show a) => Widget a -> m String
showWidget wRef = show <$> (liftIO $ readIORef wRef)

instance (Show a) => Show (WidgetImpl a) where
    show w = concat [ "WidgetImpl { "
                    , show $ state w
                    , ", currentSize = "
                    , show $ currentSize w
                    , ", currentPosition = "
                    , show $ currentPosition w
                    , ", focused = "
                    , show $ focused w
                    , " }"
                    ]

growHorizontal :: (MonadIO m) => Widget a -> m Bool
growHorizontal w = do
  act <- growHorizontal_ <~ w
  st <- state <~ w
  liftIO $ act st

growVertical :: (MonadIO m) => Widget a -> m Bool
growVertical w = do
  act <- growVertical_ <~ w
  st <- state <~ w
  liftIO $ act st

render :: (MonadIO m, Show a) => Widget a -> DisplayRegion -> RenderContext -> m Image
render wRef sz ctx =
    liftIO $ do
      impl <- readIORef wRef

      -- Merge the override attributes with the context.  If the
      -- overrides haven't been set (still def_attr), they will have
      -- no effect on the context attributes.
      norm <- normalAttribute <~ wRef
      foc <- focusAttribute <~ wRef
      let newCtx = ctx { normalAttr = mergeAttr norm $ normalAttr ctx
                       , focusAttr = mergeAttr foc $ focusAttr ctx
                       }

      img <- render_ impl wRef sz newCtx
      let imgsz =  DisplayRegion (image_width img) (image_height img)
      when (image_width img > region_width sz ||
            image_height img > region_height sz) $ throw $ ImageTooBig (show impl) sz imgsz
      setCurrentSize wRef $ DisplayRegion (image_width img) (image_height img)
      return img

renderAndPosition :: (MonadIO m, Show a) => Widget a -> DisplayRegion -> DisplayRegion
                  -> RenderContext -> m Image
renderAndPosition wRef pos sz ctx = do
  img <- render wRef sz ctx
  -- Position post-processing depends on the sizes being correct!
  setCurrentPosition wRef pos
  return img

setCurrentSize :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setCurrentSize wRef newSize =
    liftIO $ modifyIORef wRef $ \w -> w { currentSize = newSize }

getCurrentSize :: (MonadIO m) => Widget a -> m DisplayRegion
getCurrentSize wRef = (return . currentSize) =<< (liftIO $ readIORef wRef)

getCurrentPosition :: (MonadIO m, Functor m) => Widget a -> m DisplayRegion
getCurrentPosition wRef = currentPosition <$> (liftIO $ readIORef wRef)

setCurrentPosition :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setCurrentPosition wRef pos = do
  updateWidget wRef $ \w -> w { currentPosition = pos }
  liftIO $ do
    w <- readIORef wRef
    (setCurrentPosition_ w) wRef pos

newWidget :: (MonadIO m) => (WidgetImpl a -> WidgetImpl a) -> m (Widget a)
newWidget f = do
  gfhs <- newHandlers
  lfhs <- newHandlers

  wRef <- liftIO $ newIORef $
          WidgetImpl { state = undefined
                     , render_ = undefined
                     , growVertical_ = const $ return False
                     , growHorizontal_ = const $ return False
                     , setCurrentPosition_ = \_ _ -> return ()
                     , currentSize = DisplayRegion 0 0
                     , currentPosition = DisplayRegion 0 0
                     , focused = False
                     , gainFocusHandlers = gfhs
                     , loseFocusHandlers = lfhs
                     , keyEventHandler = \_ _ _ -> return False
                     , getCursorPosition_ = defaultCursorInfo
                     , normalAttribute = def_attr
                     , focusAttribute = def_attr
                     }

  updateWidget wRef f
  return wRef

defaultCursorInfo :: Widget a -> IO (Maybe DisplayRegion)
defaultCursorInfo w = do
  sz <- getCurrentSize w
  pos <- getCurrentPosition w
  return $ Just $ pos `plusWidth` (region_width sz - 1)

handleKeyEvent :: (MonadIO m) => Widget a -> Key -> [Modifier] -> m Bool
handleKeyEvent wRef keyEvent mods = do
  act <- keyEventHandler <~ wRef
  liftIO $ act wRef keyEvent mods

relayKeyEvents :: (MonadIO m) => Widget a -> Widget b -> m ()
relayKeyEvents a b = a `onKeyPressed` \_ k mods -> handleKeyEvent b k mods

relayFocusEvents :: (MonadIO m) => Widget a -> Widget b -> m ()
relayFocusEvents a b = do
  a `onGainFocus` \_ -> focus b
  a `onLoseFocus` \_ -> unfocus b

onKeyPressed :: (MonadIO m) => Widget a -> (Widget a -> Key -> [Modifier] -> IO Bool) -> m ()
onKeyPressed wRef handler = do
  -- Create a new handler that calls this one but defers to the old
  -- one if the new one doesn't handle the event.
  oldHandler <- keyEventHandler <~ wRef

  let combinedHandler =
          \w k ms -> do
            v <- handler w k ms
            case v of
              True -> return True
              False -> oldHandler w k ms

  updateWidget wRef $ \w -> w { keyEventHandler = combinedHandler }

focus :: (MonadIO m) => Widget a -> m ()
focus wRef = do
  updateWidget wRef $ \w -> w { focused = True }
  fireEvent wRef (gainFocusHandlers <~) wRef

unfocus :: (MonadIO m) => Widget a -> m ()
unfocus wRef = do
  updateWidget wRef $ \w -> w { focused = False }
  fireEvent wRef (loseFocusHandlers <~) wRef

onGainFocus :: (MonadIO m) => Widget a -> (Widget a -> IO ()) -> m ()
onGainFocus = addHandler (gainFocusHandlers <~)

onLoseFocus :: (MonadIO m) => Widget a -> (Widget a -> IO ()) -> m ()
onLoseFocus = addHandler (loseFocusHandlers <~)

(<~) :: (MonadIO m) => (a -> b) -> IORef a -> m b
(<~) f wRef = (return . f) =<< (liftIO $ readIORef wRef)

(<~~) :: (MonadIO m) => (a -> b) -> Widget a -> m b
(<~~) f wRef = (return . f . state) =<< (liftIO $ readIORef wRef)

updateWidget :: (MonadIO m) => Widget a -> (WidgetImpl a -> WidgetImpl a) -> m ()
updateWidget wRef f = (liftIO $ modifyIORef wRef f)

getState :: (MonadIO m) => Widget a -> m a
getState wRef = state <~ wRef

updateWidgetState :: (MonadIO m) => Widget a -> (a -> a) -> m ()
updateWidgetState wRef f =
    liftIO $ do
      w <- readIORef wRef
      writeIORef wRef $ w { state = f (state w) }

data FocusGroupError = FocusGroupEmpty
                     | FocusGroupBadIndex Int
                       deriving (Typeable, Show)

instance Exception FocusGroupError

data FocusEntry = forall a. FocusEntry (Widget a)

data FocusGroup = FocusGroup { entries :: [Widget FocusEntry]
                             , currentEntryNum :: Int
                             , nextKey :: (Key, [Modifier])
                             , prevKey :: (Key, [Modifier])
                             }

newFocusEntry :: (MonadIO m, Show a) => Widget a -> m (Widget FocusEntry)
newFocusEntry chRef = do
  wRef <- newWidget $ \w ->
      w { state = FocusEntry chRef

        , growHorizontal_ = const $ growHorizontal chRef
        , growVertical_ = const $ growVertical chRef

        , render_ =
            \_ sz ctx -> render chRef sz ctx

        , setCurrentPosition_ =
            \this pos -> do
              (FocusEntry ch) <- getState this
              setCurrentPosition ch pos
        }

  wRef `relayFocusEvents` chRef
  wRef `relayKeyEvents` chRef

  return wRef

newFocusGroup :: (MonadIO m) => m (Widget FocusGroup)
newFocusGroup = do
  wRef <- newWidget $ \w ->
      w { state = FocusGroup { entries = []
                             , currentEntryNum = -1
                             , nextKey = (KASCII '\t', [])
                             , prevKey = (KASCII '\t', [MShift])
                             }

        , getCursorPosition_ =
            \this -> do
              eRef <- currentEntry this
              (FocusEntry e) <- state <~ eRef
              getCursorPosition e

        , keyEventHandler =
            \this key mods -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return False
                i -> do
                  if (key, mods) == nextKey st then
                      (focusNext this >> return True) else
                      if (key, mods) == prevKey st then
                            (focusPrevious this >> return True) else
                          do
                            let e = entries st !! i
                            handleKeyEvent e key mods

        -- Should never be rendered.
        , render_ = \_ _ _ -> return empty_image
        }
  return wRef

setFocusGroupNextKey :: (MonadIO m) => Widget FocusGroup -> Key -> [Modifier] -> m ()
setFocusGroupNextKey fg k mods =
    updateWidgetState fg $ \s -> s { nextKey = (k, mods) }

setFocusGroupPrevKey :: (MonadIO m) => Widget FocusGroup -> Key -> [Modifier] -> m ()
setFocusGroupPrevKey fg k mods =
    updateWidgetState fg $ \s -> s { prevKey = (k, mods) }

mergeFocusGroups :: (MonadIO m) => Widget FocusGroup -> Widget FocusGroup -> m (Widget FocusGroup)
mergeFocusGroups a b = do
  c <- newFocusGroup

  aEntries <- entries <~~ a
  bEntries <- entries <~~ b

  when (null aEntries || null bEntries) $
       throw FocusGroupEmpty

  updateWidgetState c $ \s -> s { entries = aEntries ++ bEntries
                                , currentEntryNum = 0
                                }

  return c

resetFocusGroup :: (MonadIO m) => Widget FocusGroup -> m ()
resetFocusGroup fg = do
  cur <- currentEntryNum <~~ fg
  es <- entries <~~ fg
  forM_ (zip [0..] es) $ \(i, e) ->
      when (i /= cur) $ unfocus e
  when (cur >= 0) $
       focus =<< currentEntry fg

getCursorPosition :: (MonadIO m) => Widget a -> m (Maybe DisplayRegion)
getCursorPosition wRef = do
  ci <- getCursorPosition_ <~ wRef
  liftIO (ci wRef)

currentEntry :: (MonadIO m) => Widget FocusGroup -> m (Widget FocusEntry)
currentEntry wRef = do
  es <- entries <~~ wRef
  i <- currentEntryNum <~~ wRef
  when (i == -1) $ throw FocusGroupEmpty
  return (es !! i)

addToFocusGroup :: (MonadIO m, Show a) => Widget FocusGroup -> Widget a -> m (Widget FocusEntry)
addToFocusGroup cRef wRef = do
  eRef <- newFocusEntry wRef

  entryPos <- (length . entries) <~~ cRef
  updateWidgetState cRef $ \s -> s { entries = (entries s) ++ [eRef] }

  -- Add an event handler to the widget, NOT the entry wrapper, so
  -- others can call 'focus' on the widget and affect this focus
  -- group.
  wRef `onGainFocus` \_ ->
      setCurrentFocus cRef entryPos

  -- If we just added the first widget to the group, focus it so
  -- something has focus.
  when (entryPos == 0) $ focus eRef

  return eRef

focusNext :: (MonadIO m) => Widget FocusGroup -> m ()
focusNext wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  let nextEntry = if cur < length (entries st) - 1 then
                      (entries st) !! (cur + 1) else
                      (entries st) !! 0
  focus nextEntry

focusPrevious :: (MonadIO m) => Widget FocusGroup -> m ()
focusPrevious wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  let prevEntry = if cur > 0 then
                      (entries st) !! (cur - 1) else
                      (entries st) !! (length (entries st) - 1)
  focus prevEntry

-- Note that this only 1) updates the focus index in the group and 2)
-- calls unfocus on the previously-focused widget.  This does NOT call
-- focus on the newly-focused widget, because this is intended to be
-- callable from a focus event handler for the widget that got
-- focused.
setCurrentFocus :: (MonadIO m) => Widget FocusGroup -> Int -> m ()
setCurrentFocus cRef i = do
  st <- state <~ cRef

  when (i >= length (entries st) || i < 0) $
       throw $ FocusGroupBadIndex i

  -- If new entry number is different from existing one, invoke focus
  -- handlers.
  when (currentEntryNum st /= i) $
       do
         when (currentEntryNum st >= 0) $
              unfocus ((entries st) !! (currentEntryNum st))

  updateWidgetState cRef $ \s -> s { currentEntryNum = i }
