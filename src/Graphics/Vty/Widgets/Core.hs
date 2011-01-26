{-# LANGUAGE CPP, ExistentialQuantification, DeriveDataTypeable #-}
-- |This module provides a basic infrastructure for modelling a user
-- interface widget and converting it to Vty's 'Image' type.
module Graphics.Vty.Widgets.Core
    ( WidgetImpl(..)
    , Widget
    , RenderContext(..)
    , getNormalAttr
    , defaultContext
    , renderAndPosition
    , render
    , updateWidget
    , updateWidgetState
    , newWidget
    , getState
    , getPhysicalSize
    , setPhysicalPosition
    , getPhysicalPosition
    , showWidget
    , (<~)
    , (<~~)

    -- ** Miscellaneous
    , HasNormalAttr(..)
    , HasFocusAttr(..)
    , withNormalAttribute
    , withFocusAttribute

    , growVertical
    , growHorizontal

    -- ** Events
    , handleKeyEvent
    , onKeyPressed
    , onGainFocus
    , onLoseFocus

    -- ** Focus management
    , FocusGroup
    , FocusGroupError(..)
    , newFocusGroup
    , addToFocusGroup
    , focusNext
    , focusPrevious
    , setCurrentFocus
    , getCursorPosition
    , setFocusGroup
    , getFocusGroup
    , focus
    , unfocus
    )
where

import Data.Typeable
    ( Typeable
    )
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , modifyIORef
    , writeIORef
    )
import Control.Applicative
    ( (<$>)
    )
import Control.Monad
    ( when
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Control.Exception
    ( Exception
    , throw
    )
import Graphics.Vty
    ( DisplayRegion(DisplayRegion)
    , Image
    , Attr
    , Key(..)
    , Modifier(..)
    , def_attr
    , image_width
    , image_height
    , region_width
    , region_height
    , empty_image
    )
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Skins

class HasNormalAttr a where
    setNormalAttribute :: (MonadIO m) => a -> Attr -> m ()

class HasFocusAttr a where
    setFocusAttribute :: (MonadIO m) => a -> Attr -> m ()

withNormalAttribute :: (MonadIO m, HasNormalAttr a) => Attr -> a -> m a
withNormalAttribute att w = setNormalAttribute w att >> return w

withFocusAttribute :: (MonadIO m, HasFocusAttr a) => Attr -> a -> m a
withFocusAttribute att w = setFocusAttribute w att >> return w

data RenderError = ImageTooBig String DisplayRegion DisplayRegion
                   deriving (Show, Typeable)

instance Exception RenderError where

data RenderContext = RenderContext { normalAttr :: Attr
                                   , focusAttr :: Attr
                                   , overrideAttr :: Attr
                                   , skin :: Skin
                                   }

getNormalAttr :: RenderContext -> Attr
getNormalAttr ctx = mergeAttrs [ overrideAttr ctx, normalAttr ctx ]

defaultContext :: RenderContext
defaultContext = RenderContext def_attr def_attr def_attr unicodeSkin

-- |The type of user interface widgets.  A 'Widget' provides several
-- properties:
--
-- * /Growth properties/ which provide information about how to
--   allocate space to widgets depending on their propensity to
--   consume available space
--
-- * A /rendering routine/ which converts the widget's internal state
--   into a 'Render' value.
--
-- Of primary concern is the rendering routine, 'render'.  The
-- rendering routine takes one parameter: the size of the space in
-- which the widget should be rendered.  The space is important
-- because it provides a maximum size for the widget.  For widgets
-- that consume all available space, the size of the resulting
-- 'Render' will be equal to the supplied size.  For smaller widgets
-- (e.g., a simple string of text), the size of the 'Render' will
-- likely be much smaller than the supplied size.  In any case, any
-- 'Widget' implementation /must/ obey the rule that the resulting
-- 'Render' must not exceed the supplied 'DisplayRegion' in size.  If
-- it does, there's a good chance your interface will be garbled.
--
-- If the widget has child widgets, the supplied size should be
-- subdivided to fit the child widgets as appropriate.  How the space
-- is subdivided may depend on the growth properties of the children
-- or it may be a matter of policy.
data WidgetImpl a = WidgetImpl {
    -- |User-defined state type.
      state :: a

    -- |Render the widget with the given dimensions.  The result
    -- /must/ not be larger than the specified dimensions, but may be
    -- smaller.
    , draw :: Widget a -> DisplayRegion -> RenderContext -> IO Image

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    , getGrowHorizontal :: a -> IO Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    , getGrowVertical :: a -> IO Bool

    , physicalSize :: DisplayRegion
    , physicalPosition :: DisplayRegion

    , setPosition :: Widget a -> DisplayRegion -> IO ()

    , keyEventHandler :: Widget a -> Key -> [Modifier] -> IO Bool

    , gainFocusHandler :: Widget a -> IO ()
    , loseFocusHandler :: Widget a -> IO ()
    , focused :: Bool
    , focusGroup :: Widget a -> IO (Maybe (Widget FocusGroup))

    , cursorInfo :: Widget a -> IO (Maybe DisplayRegion)
    }

type Widget a = IORef (WidgetImpl a)

showWidget :: (Functor m, MonadIO m, Show a) => Widget a -> m String
showWidget wRef = show <$> (liftIO $ readIORef wRef)

instance (Show a) => Show (WidgetImpl a) where
    show w = concat $ [ "WidgetImpl { "
                      , show $ state w
                      , ", physicalSize = "
                      , show $ physicalSize w
                      , ", physicalPosition = "
                      , show $ physicalPosition w
                      , ", focused = "
                      , show $ focused w
                      , " }"
                      ]

setFocusGroup :: (MonadIO m) => Widget a -> Widget FocusGroup -> m ()
setFocusGroup wRef fg =
    updateWidget wRef $ \w -> w { focusGroup = const $ return $ Just fg }

getFocusGroup :: (MonadIO m) => Widget a -> m (Maybe (Widget FocusGroup))
getFocusGroup wRef = do
  act <- focusGroup <~ wRef
  liftIO $ act wRef

growHorizontal :: (MonadIO m) => Widget a -> m Bool
growHorizontal w = do
  act <- getGrowHorizontal <~ w
  st <- state <~ w
  liftIO $ act st

growVertical :: (MonadIO m) => Widget a -> m Bool
growVertical w = do
  act <- getGrowVertical <~ w
  st <- state <~ w
  liftIO $ act st

render :: (MonadIO m, Show a) => Widget a -> DisplayRegion -> RenderContext -> m Image
render wRef sz ctx =
    liftIO $ do
      impl <- readIORef wRef
      img <- draw impl wRef sz ctx
      let imgsz =  DisplayRegion (image_width img) (image_height img)
      when (image_width img > region_width sz ||
            image_height img > region_height sz) $ throw $ ImageTooBig (show impl) sz imgsz
      setPhysicalSize wRef $ DisplayRegion (image_width img) (image_height img)
      return img

renderAndPosition :: (MonadIO m, Show a) => Widget a -> DisplayRegion -> DisplayRegion
                  -> RenderContext -> m Image
renderAndPosition wRef pos sz ctx = do
  img <- render wRef sz ctx
  -- Position post-processing depends on the sizes being correct!
  setPhysicalPosition wRef pos
  return img

setPhysicalSize :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setPhysicalSize wRef newSize =
    liftIO $ modifyIORef wRef $ \w -> w { physicalSize = newSize }

getPhysicalSize :: (MonadIO m) => Widget a -> m DisplayRegion
getPhysicalSize wRef = (return . physicalSize) =<< (liftIO $ readIORef wRef)

getPhysicalPosition :: (MonadIO m, Functor m) => Widget a -> m DisplayRegion
getPhysicalPosition wRef = physicalPosition <$> (liftIO $ readIORef wRef)

setPhysicalPosition :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setPhysicalPosition wRef pos = do
  updateWidget wRef $ \w -> w { physicalPosition = pos }
  liftIO $ do
    w <- readIORef wRef
    (setPosition w) wRef pos

newWidget :: (MonadIO m) => m (Widget a)
newWidget =
    liftIO $ newIORef $ WidgetImpl { state = undefined
                                   , draw = undefined
                                   , getGrowVertical = const $ return False
                                   , getGrowHorizontal = const $ return False
                                   , keyEventHandler = \_ _ _ -> return False
                                   , physicalSize = DisplayRegion 0 0
                                   , physicalPosition = DisplayRegion 0 0
                                   , gainFocusHandler =
                                       \this -> updateWidget this $ \w -> w { focused = True }
                                   , loseFocusHandler =
                                       \this -> updateWidget this $ \w -> w { focused = False }
                                   , focused = False
                                   , cursorInfo = const $ return Nothing
                                   , setPosition = \_ _ -> return ()
                                   , focusGroup = const $ return Nothing
                                   }

handleKeyEvent :: (MonadIO m) => Widget a -> Key -> [Modifier] -> m Bool
handleKeyEvent wRef keyEvent mods = do
  act <- keyEventHandler <~ wRef
  liftIO $ act wRef keyEvent mods

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
  act <- gainFocusHandler <~ wRef
  liftIO $ act wRef

unfocus :: (MonadIO m) => Widget a -> m ()
unfocus wRef = do
  act <- loseFocusHandler <~ wRef
  liftIO $ act wRef

onGainFocus :: (MonadIO m) => Widget a -> (Widget a -> IO ()) -> m ()
onGainFocus wRef handler = do
  oldHandler <- gainFocusHandler <~ wRef
  let combinedHandler = \w -> oldHandler w >> handler w
  updateWidget wRef $ \w -> w { gainFocusHandler = combinedHandler }

onLoseFocus :: (MonadIO m) => Widget a -> (Widget a -> IO ()) -> m ()
onLoseFocus wRef handler = do
  oldHandler <- loseFocusHandler <~ wRef
  let combinedHandler = \w -> oldHandler w >> handler w
  updateWidget wRef $ \w -> w { loseFocusHandler = combinedHandler }

(<~) :: (MonadIO m) => (WidgetImpl a -> b) -> Widget a -> m b
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
                             }

newFocusEntry :: (MonadIO m, Show a) =>
                 Widget a
              -> m (Widget FocusEntry)
newFocusEntry chRef = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = FocusEntry chRef

        , getGrowHorizontal = const $ growHorizontal chRef
        , getGrowVertical = const $ growVertical chRef

        , draw =
            \_ sz ctx -> render chRef sz ctx

        , setPosition =
            \this pos -> do
              (FocusEntry ch) <- getState this
              setPhysicalPosition ch pos
        }

  wRef `onLoseFocus` (const $ unfocus chRef)
  wRef `onGainFocus` (const $ focus chRef)
  wRef `onKeyPressed` (\_ k -> handleKeyEvent chRef k)

  return wRef

newFocusGroup :: (MonadIO m) => m (Widget FocusGroup)
newFocusGroup = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = FocusGroup { entries = []
                             , currentEntryNum = -1
                             }
        , keyEventHandler =
            \this key mods -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return False
                i -> do
                  case (key, mods) of
                    (KASCII '\t', []) -> do
                             focusNext this
                             return True
                    (KASCII '\t', [MShift]) -> do
                             focusPrevious this
                             return True
                    _ -> do
                       let e = entries st !! i
                       handleKeyEvent e key mods

        -- Should never be rendered.
        , draw = \_ _ _ -> return empty_image
        }
  return wRef

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
  when (i == -1) $ throw FocusGroupEmpty
  return (es !! i)

addToFocusGroup :: (MonadIO m, Show a) => Widget FocusGroup -> Widget a -> m (Widget FocusEntry)
addToFocusGroup cRef wRef = do
  eRef <- newFocusEntry wRef
  updateWidgetState cRef $ \s -> s { entries = (entries s) ++ [eRef] }

  -- If we just added the first widget to the group, focus it so
  -- something has focus.
  numEntries <- (length . entries) <~~ cRef
  when (numEntries == 1) $ do
    updateWidgetState cRef $ \s -> s { currentEntryNum = 0 }
    focus eRef

  return eRef

focusNext :: (MonadIO m) => Widget FocusGroup -> m ()
focusNext wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  if cur < length (entries st) - 1 then
      setCurrentFocus wRef (cur + 1) else
      setCurrentFocus wRef 0

focusPrevious :: (MonadIO m) => Widget FocusGroup -> m ()
focusPrevious wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  if cur > 0 then
      setCurrentFocus wRef (cur - 1) else
      setCurrentFocus wRef (length (entries st) - 1)

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
         focus ((entries st) !! i)

  updateWidgetState cRef $ \s -> s { currentEntryNum = i }
