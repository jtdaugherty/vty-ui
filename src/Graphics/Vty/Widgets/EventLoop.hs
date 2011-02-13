{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Vty.Widgets.EventLoop
    ( EventLoopError(..)
    , runUi
    , schedule
    )
where

import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Vty
import Graphics.Vty.Widgets.Core

data EventLoopError = NoFocusGroup
                      deriving (Show, Typeable)

instance Exception EventLoopError

data CombinedEvent = VTYEvent Event
                   | UserEvent UserEvent

data UserEvent = ScheduledAction (IO ())

eventChan :: Chan CombinedEvent
{-# NOINLINE eventChan #-}
eventChan = unsafePerformIO newChan

runUi :: (MonadIO m, Show a) => Widget a -> RenderContext -> m ()
runUi uiWidget ctx =
    liftIO $ do
      vty <- mkVty

      -- Create VTY event listener thread
      _ <- forkIO $ vtyEventListener vty eventChan

      runUi' vty eventChan uiWidget ctx `finally` do
               reserve_display $ terminal vty
               shutdown vty

vtyEventListener :: Vty -> Chan CombinedEvent -> IO ()
vtyEventListener vty chan =
    forever $ do
      e <- next_event vty
      writeChan chan $ VTYEvent e

schedule :: (MonadIO m) => IO () -> m ()
schedule act = liftIO $ writeChan eventChan $ UserEvent $ ScheduledAction act

runUi' :: (Show a) => Vty -> Chan CombinedEvent -> Widget a -> RenderContext -> IO ()
runUi' vty chan uiWidget ctx = do
  sz <- display_bounds $ terminal vty
  img <- renderAndPosition uiWidget (DisplayRegion 0 0) sz ctx
  update vty $ pic_for_image img

  mPos <- getCursorPosition uiWidget
  case mPos of
    Just (DisplayRegion w h) -> do
                        show_cursor $ terminal vty
                        set_cursor_pos (terminal vty) w h
    Nothing -> hide_cursor $ terminal vty

  evt <- readChan chan

  case evt of
    VTYEvent (EvKey k mods) -> handleKeyEvent uiWidget k mods >> return ()
    UserEvent (ScheduledAction act) -> liftIO act
    _ -> return ()

  runUi' vty chan uiWidget ctx
