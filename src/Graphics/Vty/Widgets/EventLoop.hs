{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
-- |This module provides the main event loop functionality for this
-- library.  All vty-ui applications must use runUi to get anything
-- done usefully.
module Graphics.Vty.Widgets.EventLoop
    ( Collection
    , CollectionError(..)
    , runUi
    , schedule
    , shutdownUi
    , newCollection
    , addToCollection
    , addToCollectionWithCallbacks
    , setCurrentEntry
    )
where

import Data.IORef
import Data.Typeable
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Vty
import Graphics.Vty.Widgets.Core

data CombinedEvent = VTYEvent Event
                   | UserEvent UserEvent
                   | ShutdownUi

data UserEvent = ScheduledAction (IO ())

eventChan :: TChan CombinedEvent
{-# NOINLINE eventChan #-}
eventChan = unsafePerformIO newTChanIO

-- |Run the main vty-ui event loop using the specified interface
-- collection and initial rendering context.  The rendering context
-- provides the default attributes and 'Skin' to use for the
-- application.  Throws 'BadCollectionIndex' if the specified
-- 'Collection' is empty.
runUi :: Collection -> RenderContext -> IO ()
runUi collection ctx = do
  vty <- mkVty

  -- Create VTY event listener thread
  _ <- forkIO $ vtyEventListener vty eventChan

  setCurrentEntry collection 0
  runUi' vty eventChan collection ctx `finally` shutdown vty

vtyEventListener :: Vty -> TChan CombinedEvent -> IO ()
vtyEventListener vty chan =
    forever $ do
      e <- next_event vty
      atomically $ writeTChan chan $ VTYEvent e

-- |Schedule a widget-mutating 'IO' action to be run by the main event
-- loop.  Use of this function is required to guarantee consistency
-- between interface presentation and internal state.
schedule :: IO () -> IO ()
schedule act = atomically $ writeTChan eventChan $ UserEvent $ ScheduledAction act

-- |Schedule a vty-ui event loop shutdown.  This event will preempt
-- others so that it will be processed next.
shutdownUi :: IO ()
shutdownUi = atomically $ unGetTChan eventChan ShutdownUi

runUi' :: Vty -> TChan CombinedEvent -> Collection -> RenderContext -> IO ()
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

  -- Get the next event(s) in the queue.  Returns all available events;
  -- blocks until at least one event is available.
  let getNextEvents = do
      evt <- readTChan chan
      em <- isEmptyTChan chan
      case em of
          True -> return [evt]
          False -> do
              rest <- getNextEvents
              return $ evt : rest

  evts <- atomically getNextEvents

  let processEvent lastCont evt = do
          if not lastCont then
              return False else
              case evt of
                  VTYEvent (EvKey k mods) -> handleKeyEvent fg k mods >> return True
                  VTYEvent _ -> return True
                  UserEvent (ScheduledAction act) -> act >> return True
                  ShutdownUi -> return False

  cont <- foldM processEvent True evts

  when cont $ runUi' vty chan collection ctx

data CollectionError = BadCollectionIndex Int
                       deriving (Show, Typeable)

instance Exception CollectionError

type EntryShow = IO ()
type EntryHide = IO ()

data Entry = forall a. (Show a) => Entry
    { entryWidget :: Widget a
    , entryFocusGroup :: Widget FocusGroup
    , entryShowCallback :: EntryShow
    , entryHideCallback :: EntryHide
    }

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

entryRenderAndPosition :: Entry -> DisplayRegion -> DisplayRegion -> RenderContext -> IO Image
entryRenderAndPosition (Entry { entryWidget = w }) = renderAndPosition w

-- |Create a new collection.
newCollection :: IO Collection
newCollection =
    newIORef $ CollectionData { entries = []
                              , currentEntryNum = -1
                              }

getMaybeCurrentEntry :: Collection -> IO (Maybe Entry)
getMaybeCurrentEntry cRef = do
  cur <- currentEntryNum <~ cRef
  es <- entries <~ cRef
  if cur == -1
    then return Nothing
    else if cur >= 0 && cur < length es
      then return . Just $ es !! cur
      else return Nothing

getCurrentEntry :: Collection -> IO Entry
getCurrentEntry cRef = do
  maybeEntry <- getMaybeCurrentEntry cRef
  cur <- currentEntryNum <~ cRef
  case maybeEntry of
    Nothing -> throw $ BadCollectionIndex cur
    Just entry -> return entry

-- |Add a widget and its focus group to a collection.  Returns an
-- action which, when invoked, will switch to the interface specified
-- in the call.
addToCollection :: (Show a) => Collection -> Widget a -> Widget FocusGroup -> IO (IO ())
addToCollection cRef wRef fg = addToCollectionWithCallbacks cRef wRef fg (return ()) (return ())

addToCollectionWithCallbacks :: (Show a) => Collection -> Widget a -> Widget FocusGroup -> EntryShow -> EntryHide -> IO (IO ())
addToCollectionWithCallbacks cRef wRef fg onShowCb onHideCb = do
  i <- (length . entries) <~ cRef
  modifyIORef cRef $ \st ->
      st { entries = (entries st) ++ [Entry wRef fg onShowCb onHideCb]
         , currentEntryNum = if currentEntryNum st == -1
                             then 0
                             else currentEntryNum st
         }
  resetFocusGroup fg
  return $ setCurrentEntry cRef i

setCurrentEntry :: Collection -> Int -> IO ()
setCurrentEntry cRef i = do
  st <- readIORef cRef

  if i < length (entries st) && i >= 0
    then do
      -- Let the old entry know it's no longer current.
      maybeOldEntry <- getMaybeCurrentEntry cRef
      case maybeOldEntry of
        Nothing -> return ()
        Just oldEntry -> entryHideCallback oldEntry
      -- Set the current entry index.
      (modifyIORef cRef $ \s -> s { currentEntryNum = i })
    else throw $ BadCollectionIndex i

  e <- getCurrentEntry cRef
  entryShowCallback e
  resetFocusGroup $ entryFocusGroup e
