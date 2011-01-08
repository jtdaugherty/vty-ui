{-# LANGUAGE CPP #-}
-- |This module provides a basic infrastructure for modelling a user
-- interface widget and converting it to Vty's 'Image' type.
module Graphics.Vty.Widgets.Rendering
    ( WidgetImpl(..)
    , Widget
    , render
    , updateWidget
    , updateWidget_
    , updateWidgetState
    , updateWidgetState_
    , newWidget
    , getState
    , getPhysicalSize
    , getPhysicalPosition
    , (<~)
    , (<~~)

    -- ** Miscellaneous
    , Orientation(..)
    , withWidth
    , withHeight

    , growVertical
    , growHorizontal

    -- ** Events
    , handleKeyEvent
    , onKeyPressed
    )
where

import GHC.Word ( Word )
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
import Control.Monad.Reader
    ( ReaderT
    , runReaderT
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
    ( DisplayRegion(DisplayRegion)
    , Image
    , Attr
    , Key
    , image_width
    , image_height
    )

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

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
    , draw :: Widget a -> DisplayRegion -> DisplayRegion -> Maybe Attr
           -> IO Image

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    , getGrowHorizontal :: ReaderT a IO Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    , getGrowVertical :: ReaderT a IO Bool

    , physicalSize :: DisplayRegion
    , physicalPosition :: DisplayRegion

    , keyEventHandler :: Widget a -> Key -> IO Bool
    }

type Widget a = IORef (WidgetImpl a)

growHorizontal :: (MonadIO m) => Widget a -> m Bool
growHorizontal w = do
  act <- getGrowHorizontal <~ w
  st <- state <~ w
  liftIO $ runReaderT act st

growVertical :: (MonadIO m) => Widget a -> m Bool
growVertical w = do
  act <- getGrowVertical <~ w
  st <- state <~ w
  liftIO $ runReaderT act st

render :: (MonadIO m) => Widget a -> DisplayRegion -> DisplayRegion -> Maybe Attr -> m Image
render wRef pos sz overrideAttr =
    liftIO $ do
      impl <- readIORef wRef
      -- Set the position first, in case it needs to be used by the
      -- drawing routine.
      setPhysicalPosition wRef pos
      img <- draw impl wRef (DisplayRegion 0 0) sz overrideAttr
      -- But we can't set the size until after drawing is done.
      setPhysicalSize wRef $ DisplayRegion (image_width img) (image_height img)
      return img

setPhysicalSize :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setPhysicalSize wRef newSize =
    liftIO $ modifyIORef wRef $ \w -> w { physicalSize = newSize }

getPhysicalSize :: (MonadIO m, Functor m) => Widget a -> m DisplayRegion
getPhysicalSize wRef = physicalSize <$> (liftIO $ readIORef wRef)

setPhysicalPosition :: (MonadIO m) => Widget a -> DisplayRegion -> m ()
setPhysicalPosition wRef newPos =
    liftIO $ modifyIORef wRef $ \w -> w { physicalPosition = newPos }

getPhysicalPosition :: (MonadIO m, Functor m) => Widget a -> m DisplayRegion
getPhysicalPosition wRef = physicalPosition <$> (liftIO $ readIORef wRef)

newWidget :: (MonadIO m) => m (Widget a)
newWidget =
    liftIO $ newIORef $ WidgetImpl { state = undefined
                                   , draw = undefined
                                   , getGrowVertical = undefined
                                   , getGrowHorizontal = undefined
                                   , keyEventHandler = \_ _ -> return False
                                   , physicalSize = DisplayRegion 0 0
                                   , physicalPosition = DisplayRegion 0 0
                                   }

handleKeyEvent :: (MonadIO m) => Widget a -> Key -> m Bool
handleKeyEvent wRef keyEvent = do
  act <- keyEventHandler <~ wRef
  liftIO $ act wRef keyEvent

onKeyPressed :: (MonadIO m) => Widget a -> (Widget a -> Key -> IO Bool) -> m ()
onKeyPressed wRef handler = do
  -- Create a new handler that calls this one but defers to the old
  -- one if the new one doesn't handle the event.
  oldHandler <- keyEventHandler <~ wRef

  let combinedHandler =
          \w k -> do
            v <- handler w k
            case v of
              True -> return True
              False -> oldHandler w k

  updateWidget_ wRef $ \w -> w { keyEventHandler = combinedHandler }

(<~) :: (MonadIO m) => (WidgetImpl a -> b) -> Widget a -> m b
(<~) f wRef = (return . f) =<< (liftIO $ readIORef wRef)

(<~~) :: (MonadIO m) => (a -> b) -> Widget a -> m b
(<~~) f wRef = (return . f . state) =<< (liftIO $ readIORef wRef)

updateWidget :: (MonadIO m) => Widget a -> (WidgetImpl a -> WidgetImpl a) -> m (Widget a)
updateWidget wRef f = (liftIO $ modifyIORef wRef f) >> return wRef

updateWidget_ :: (MonadIO m) => Widget a -> (WidgetImpl a -> WidgetImpl a) -> m ()
updateWidget_ wRef f = updateWidget wRef f >> return ()

getState :: (MonadIO m) => Widget a -> m a
getState wRef = state <~ wRef

updateWidgetState :: (MonadIO m) => Widget a -> (a -> a) -> m (Widget a)
updateWidgetState wRef f =
    liftIO $ do
      w <- readIORef wRef
      writeIORef wRef $ w { state = f (state w) }
      return wRef

updateWidgetState_ :: (MonadIO m) => Widget a -> (a -> a) -> m ()
updateWidgetState_ wRef f = updateWidgetState wRef f >> return ()

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h
