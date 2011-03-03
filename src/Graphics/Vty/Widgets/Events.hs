-- |This module provides infrastructure for widgets that need to
-- produce events and provide event handler registration
-- functionality.
module Graphics.Vty.Widgets.Events
    ( Handlers
    , Handler
    , newHandlers
    , addHandler
    , fireEvent
    )
where

import Control.Monad.Trans
import Control.Monad
import Data.IORef

-- |The type of event handlers which take a parameter of type 'a'.
type Handler a = a -> IO ()

-- |The type of event handler collections of parameter type 'a'.
newtype Handlers a = Handlers (IORef [Handler a])

-- |Given an event handler collection projection combinator, a target,
-- and a handler, add the handler to the target's event handler
-- collection.
addHandler :: (MonadIO m) => (w -> m (Handlers a)) -> w -> Handler a -> m ()
addHandler getRef w handler = do
  (Handlers r) <- getRef w
  liftIO $ modifyIORef r $ \s -> s ++ [handler]

-- |Fire an event by extracting an event handler collection from a
-- target and invoking all of its handlers with the specified
-- parameter value.
fireEvent :: (MonadIO m) => w -> (w -> m (Handlers a)) -> a -> m ()
fireEvent w getRef ev = do
  (Handlers r) <- getRef w
  handlers <- liftIO $ readIORef r
  forM_ handlers $ \handler ->
      liftIO $ handler ev

-- |Create a new event handler collection.
newHandlers :: (MonadIO m) => m (Handlers a)
newHandlers = do
  r <- liftIO $ newIORef []
  return $ Handlers r
