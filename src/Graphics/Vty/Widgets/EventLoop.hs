{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module Graphics.Vty.Widgets.EventLoop
    ( EventLoopError(..)
    , Collection
    , CollectionError(..)
    , runUi
    , schedule
    , newCollection
    , addToCollection
    )
where

import Data.IORef
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

data CollectionError = EmptyCollection
                     | BadCollectionIndex Int
                       deriving (Show, Typeable)

instance Exception CollectionError

data Entry = forall a. (Show a) => Entry (Widget a) (Widget FocusGroup)

data CollectionData =
    CollectionData { entries :: [Entry]
                   , currentEntryNum :: Int
                   }

type Collection = IORef CollectionData

instance Show CollectionData where
    show (CollectionData es num) = concat [ "Collection { "
                                          , "entries = <", show $ length es, "entries>"
                                          , ", currentEntryNum = ", show num
                                          , " }"
                                          ]

entryRenderAndPosition :: (MonadIO m) => Entry -> DisplayRegion -> DisplayRegion -> RenderContext -> m Image
entryRenderAndPosition (Entry w _) = renderAndPosition w

entryFocusGroup :: Entry -> Widget FocusGroup
entryFocusGroup (Entry _ fg) = fg

newCollection :: (MonadIO m) => m Collection
newCollection =
    liftIO $ newIORef $ CollectionData { entries = []
                                       , currentEntryNum = -1
                                       }

getCurrentEntry :: (MonadIO m) => Collection -> m Entry
getCurrentEntry cRef = do
  cur <- currentEntryNum <~ cRef
  es <- entries <~ cRef
  if cur == -1 then
      throw $ BadCollectionIndex cur else
      if cur >= 0 && cur < length es then
          return $ es !! cur else
          throw $ BadCollectionIndex cur

addToCollection :: (MonadIO m, Show a) => Collection -> Widget a -> Widget FocusGroup -> m (m ())
addToCollection cRef wRef fg = do
  i <- (length . entries) <~ cRef
  liftIO $ modifyIORef cRef $ \st ->
      st { entries = (entries st) ++ [Entry wRef fg]
         , currentEntryNum = if currentEntryNum st == -1
                             then 0
                             else currentEntryNum st
         }
  resetFocusGroup fg
  return $ setCurrentEntry cRef i

setCurrentEntry :: (MonadIO m) => Collection -> Int -> m ()
setCurrentEntry cRef i = do
  st <- liftIO $ readIORef cRef
  if i < length (entries st) && i >= 0 then
      (liftIO $ modifyIORef cRef $ \s -> s { currentEntryNum = i }) else
      throw $ BadCollectionIndex i

  e <- getCurrentEntry cRef
  resetFocusGroup $ entryFocusGroup e
