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

import Control.Monad
import Data.IORef

-- |The type of event handlers which take a parameter of type 'a'.
type Handler a = a -> IO ()

-- |The type of event handler collections of parameter type 'a'.
newtype Handlers a = Handlers (IORef [Handler a])

-- |Given an event handler collection projection combinator, a target,
-- and a handler, add the handler to the target's event handler
-- collection.
addHandler :: (w -> IO (Handlers a)) -> w -> Handler a -> IO ()
addHandler getRef w handler = do
  (Handlers r) <- getRef w
  modifyIORef r $ \s -> s ++ [handler]

-- |Fire an event by extracting an event handler collection from a
-- target and invoking all of its handlers with the specified
-- parameter value.
fireEvent :: w -> (w -> IO (Handlers a)) -> a -> IO ()
fireEvent w getRef ev = do
  (Handlers r) <- getRef w
  handlers <- readIORef r
  forM_ handlers $ \handler ->
      handler ev

-- |Create a new event handler collection.
newHandlers :: IO (Handlers a)
newHandlers = do
  r <- newIORef []
  return $ Handlers r
