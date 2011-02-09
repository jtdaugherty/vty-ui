module Graphics.Vty.Widgets.ProgressBar
    ( ProgressBar(progressBarWidget)
    , newProgressBar
    , setProgress
    , addProgress
    , getProgress
    )
where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Box

data ProgressBar = ProgressBar { progressBarWidget :: Widget (Box HFill HFill)
                               , progressBarAmount :: IORef Int
                               }

newProgressBar :: (MonadIO m) => Attr -> Attr -> m ProgressBar
newProgressBar completeAttr incompleteAttr = do
  w <- (hFill ' ' 1 >>= withNormalAttribute completeAttr) <++>
       (hFill ' ' 1 >>= withNormalAttribute incompleteAttr)
  r <- liftIO $ newIORef 0
  let p = ProgressBar w r
  setProgress p 0
  return p

setProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
setProgress p amt = do
  liftIO $ writeIORef (progressBarAmount p) amt
  setBoxChildSizePolicy (progressBarWidget p) $ Percentage amt

getProgress :: (MonadIO m) => ProgressBar -> m Int
getProgress = liftIO . readIORef . progressBarAmount

addProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
addProgress p amt = do
  cur <- getProgress p
  let newAmt = cur + amt
  when (newAmt >= 0 && newAmt <= 100) $
       setProgress p newAmt