module Graphics.Vty.Widgets.Events
    ( Handler
    , mkHandlers
    , addHandler
    , fireEvent
    )
where

import Control.Monad.Trans
import Control.Monad
import Data.IORef

type Handler a = a -> IO ()

addHandler :: (MonadIO m) => (w -> IORef [Handler a]) -> w -> Handler a -> m ()
addHandler getRef w handler =
    liftIO $ modifyIORef (getRef w) $ \s -> s ++ [handler]

fireEvent :: (MonadIO m) => w -> (w -> m (IORef [Handler a])) -> a -> m ()
fireEvent w getRef ev = do
  r <- getRef w
  handlers <- liftIO $ readIORef r
  forM_ handlers $ \handler ->
      liftIO $ handler ev

mkHandlers :: (MonadIO m) => m (IORef [Handler a])
mkHandlers = liftIO $ newIORef []
