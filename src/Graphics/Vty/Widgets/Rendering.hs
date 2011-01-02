{-# LANGUAGE CPP #-}
-- |This module provides a basic infrastructure for modelling a user
-- interface widget and converting it to Vty's 'Image' type.
module Graphics.Vty.Widgets.Rendering
    ( WidgetImpl(..)
    , Widget
    , render
    , updateWidget
    , updateWidget_
    , newWidget
    , (<~)

    -- ** Miscellaneous
    , Orientation(..)
    , withWidth
    , withHeight

    , growVertical
    , growHorizontal
    )
where

import GHC.Word ( Word )
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , modifyIORef
    )
import Control.Monad.Reader
    ( ReaderT
    , runReaderT
    )
import Control.Monad.State
    ( StateT
    , runStateT
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
    ( DisplayRegion(DisplayRegion)
    , Image
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
    , draw :: DisplayRegion -> StateT a IO Image

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    , getGrowHorizontal :: ReaderT a IO Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    , getGrowVertical :: ReaderT a IO Bool
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

render :: (MonadIO m) => Widget a -> DisplayRegion -> m Image
render wRef size =
    liftIO $ do
      impl <- readIORef wRef
      (img, newState) <- runStateT (draw impl size) (state impl)
      updateWidget_ wRef $ \w -> w { state = newState }
      return img

newWidget :: (MonadIO m) => m (Widget a)
newWidget =
    liftIO $ newIORef $ WidgetImpl { state = undefined
                                   , draw = undefined
                                   , getGrowVertical = undefined
                                   , getGrowHorizontal = undefined
                                   }

(<~) :: (MonadIO m) => (WidgetImpl a -> b) -> Widget a -> m b
(<~) f wRef = (return . f) =<< (liftIO $ readIORef wRef)

updateWidget :: (MonadIO m) => Widget a -> (WidgetImpl a -> WidgetImpl a) -> m (Widget a)
updateWidget wRef f = (liftIO $ modifyIORef wRef f) >> return wRef

updateWidget_ :: (MonadIO m) => Widget a -> (WidgetImpl a -> WidgetImpl a) -> m ()
updateWidget_ wRef f = updateWidget wRef f >> return ()

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h
