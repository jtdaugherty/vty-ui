module Graphics.Vty.Widgets.Events
    ( Handlers
    , Handler
    , mkHandlers
    , addHandler
    , fireEvent
    )
where

import Control.Monad.Trans
import Control.Monad
import Data.IORef

type Handler a = a -> IO ()
newtype Handlers a = Handlers (IORef [Handler a])

addHandler :: (MonadIO m) => (w -> m (Handlers a)) -> w -> Handler a -> m ()
addHandler getRef w handler = do
  (Handlers r) <- getRef w
  liftIO $ modifyIORef r $ \s -> s ++ [handler]

fireEvent :: (MonadIO m) => w -> (w -> m (Handlers a)) -> a -> m ()
fireEvent w getRef ev = do
  (Handlers r) <- getRef w
  handlers <- liftIO $ readIORef r
  forM_ handlers $ \handler ->
      liftIO $ handler ev

mkHandlers :: (MonadIO m) => m (Handlers a)
mkHandlers = do
  r <- liftIO $ newIORef []
  return $ Handlers r
