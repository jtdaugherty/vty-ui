module Graphics.Vty.Widgets.ProgressBar
    ( ProgressBar(progressBarWidget)
    , newProgressBar
    , setProgress
    , addProgress
    , getProgress
    , onProgressChange
    )
where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Events

data ProgressBar = ProgressBar { progressBarWidget :: Widget (Box HFill HFill)
                               , progressBarAmount :: IORef Int
                               , onChangeHandlers :: Handlers Int
                               }

newProgressBar :: (MonadIO m) => Attr -> Attr -> m ProgressBar
newProgressBar completeAttr incompleteAttr = do
  w <- (hFill ' ' 1 >>= withNormalAttribute completeAttr) <++>
       (hFill ' ' 1 >>= withNormalAttribute incompleteAttr)
  r <- liftIO $ newIORef 0
  hs <- newHandlers
  let p = ProgressBar w r hs
  setProgress p 0
  return p

onProgressChange :: (MonadIO m) => ProgressBar -> (Int -> IO ()) -> m ()
onProgressChange = addHandler (return . onChangeHandlers)

setProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
setProgress p amt = do
  liftIO $ writeIORef (progressBarAmount p) amt
  setBoxChildSizePolicy (progressBarWidget p) $ Percentage amt
  fireEvent p (return . onChangeHandlers) amt

getProgress :: (MonadIO m) => ProgressBar -> m Int
getProgress = liftIO . readIORef . progressBarAmount

addProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
addProgress p amt = do
  cur <- getProgress p
  let newAmt = cur + amt
  when (newAmt >= 0 && newAmt <= 100) $
       setProgress p newAmt