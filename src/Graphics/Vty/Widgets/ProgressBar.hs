-- |This module provides a ''progress bar'' widget which stores a
-- progress value between 0 and 100 inclusive.  Use the 'schedule'
-- function to modify the progress bar's state from a thread.
module Graphics.Vty.Widgets.ProgressBar
    ( ProgressBar
    , newProgressBar
    , progressBarWidget
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
import Graphics.Vty.Widgets.Util

data ProgressBar = ProgressBar { progressBarWidget :: Widget (Box HFill HFill)
                               -- ^Get the widget of a progress bar.
                               , progressBarAmount :: IORef Int
                               , onChangeHandlers :: Handlers Int
                               }

-- |Create a new progress bar with the specified completed and
-- uncompleted colors, respectively.
newProgressBar :: (MonadIO m) => Color -> Color -> m ProgressBar
newProgressBar completeColor incompleteColor = do
  let completeAttr = completeColor `on` completeColor
      incompleteAttr = incompleteColor `on` incompleteColor

  w <- (hFill ' ' 1 >>= withNormalAttribute completeAttr) <++>
       (hFill ' ' 1 >>= withNormalAttribute incompleteAttr)
  r <- liftIO $ newIORef 0
  hs <- newHandlers
  let p = ProgressBar w r hs
  setProgress p 0
  return p

-- |Register a handler to be invoked when the progress bar's progress
-- value changes.  The handler will be passed the new progress value.
onProgressChange :: (MonadIO m) => ProgressBar -> (Int -> IO ()) -> m ()
onProgressChange = addHandler (return . onChangeHandlers)

-- |Set the progress bar's progress value.  Values outside the allowed
-- range will be ignored.
setProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
setProgress p amt =
    when (amt >= 0 && amt <= 100) $ do
      liftIO $ writeIORef (progressBarAmount p) amt
      setBoxChildSizePolicy (progressBarWidget p) $ Percentage amt
      fireEvent p (return . onChangeHandlers) amt

-- |Get the progress bar's current progress value.
getProgress :: (MonadIO m) => ProgressBar -> m Int
getProgress = liftIO . readIORef . progressBarAmount

-- |Add a delta value to the progress bar's current value.
addProgress :: (MonadIO m) => ProgressBar -> Int -> m ()
addProgress p amt = do
  cur <- getProgress p
  let newAmt = cur + amt
  when (newAmt >= 0 && newAmt <= 100) $
       setProgress p newAmt