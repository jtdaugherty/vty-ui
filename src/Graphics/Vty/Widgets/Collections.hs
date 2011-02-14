{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module Graphics.Vty.Widgets.Collections
    ( Collection
    , CollectionError(..)
    , Entry
    , newCollection
    , addToCollection
    , getCurrentEntry
    , setCurrentEntry

    , entryRenderAndPosition
    , entryFocusGroup
    )
where

import Data.Typeable
import Data.IORef
import Control.Monad.Trans
import Control.Exception
import Graphics.Vty
import Graphics.Vty.Widgets.Core

-- Ultimately we'd want support for "stacks" to provide things like
-- overlaid dialogs, but for now we'll just implement a "collection"
-- type which allows is to have a collection of widgets and switch
-- between them.

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
