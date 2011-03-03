{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
-- |This module provides the main event loop functionality for this
-- library.  All vty-ui applications must use runUi to get anything
-- done usefully.
module Graphics.Vty.Widgets.EventLoop
    ( Collection
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

data CombinedEvent = VTYEvent Event
                   | UserEvent UserEvent

data UserEvent = ScheduledAction (IO ())

eventChan :: Chan CombinedEvent
{-# NOINLINE eventChan #-}
eventChan = unsafePerformIO newChan

-- |Run the main vty-ui event loop using the specified interface
-- collection and initial rendering context.  The rendering context
-- provides the default attributes and 'Skin' to use for the
-- application.  Throws 'BadCollectionIndex' if the specified
-- 'Collection' is empty.
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

-- |Schedule a widget-mutating 'IO' action to be run by the main event
-- loop.  Use of this function is required to guarantee consistency
-- between interface presentation and internal state.
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

data CollectionError = BadCollectionIndex Int
                       deriving (Show, Typeable)

instance Exception CollectionError

data Entry = forall a. (Show a) => Entry (Widget a) (Widget FocusGroup)

data CollectionData =
    CollectionData { entries :: [Entry]
                   , currentEntryNum :: Int
                   }

-- |The type of user interface collections.
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

-- |Create a new collection.
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

-- |Add a widget and its focus group to a collection.  Returns an
-- action which, when invoked, will switch to the interface specified
-- in the call.
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
