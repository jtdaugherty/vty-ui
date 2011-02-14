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
import Graphics.Vty.Widgets.Collections

data EventLoopError = NoFocusGroup
                      deriving (Show, Typeable)

instance Exception EventLoopError

data CombinedEvent = VTYEvent Event
                   | UserEvent UserEvent

data UserEvent = ScheduledAction (IO ())

eventChan :: Chan CombinedEvent
{-# NOINLINE eventChan #-}
eventChan = unsafePerformIO newChan

runUi :: (MonadIO m) => Collection -> RenderContext -> m ()
runUi collection ctx =
    liftIO $ do
      vty <- mkVty

      -- Create VTY event listener thread
      _ <- forkIO $ vtyEventListener vty eventChan

      runUi' vty eventChan collection ctx `finally` do
               reserve_display $ terminal vty
               shutdown vty

vtyEventListener :: Vty -> Chan CombinedEvent -> IO ()
vtyEventListener vty chan =
    forever $ do
      e <- next_event vty
      writeChan chan $ VTYEvent e

schedule :: (MonadIO m) => IO () -> m ()
schedule act = liftIO $ writeChan eventChan $ UserEvent $ ScheduledAction act

runUi' :: Vty -> Chan CombinedEvent -> Collection -> RenderContext -> IO ()
runUi' vty chan collection ctx = do
  sz <- display_bounds $ terminal vty

  e <- getCurrentEntry collection
  let fg = entryFocusGroup e

  img <- entryRenderAndPosition e (DisplayRegion 0 0) sz ctx
  update vty $ pic_for_image img

  mPos <- getCursorPosition fg
  case mPos of
    Just (DisplayRegion w h) -> do
                        show_cursor $ terminal vty
                        set_cursor_pos (terminal vty) w h
    Nothing -> hide_cursor $ terminal vty

  evt <- readChan chan

  case evt of
    VTYEvent (EvKey k mods) -> handleKeyEvent fg k mods >> return ()
    UserEvent (ScheduledAction act) -> liftIO act
    _ -> return ()

  runUi' vty chan collection ctx
