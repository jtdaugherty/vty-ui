{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module Graphics.Vty.Widgets.Collections
    ( Collection
    , CollectionError(..)
    , newCollection
    , addToCollection
    , setCurrent
    )
where

import Data.Typeable
    ( Typeable
    )
import Control.Monad.Trans
    ( MonadIO
    )
import Control.Exception
    ( Exception
    , throw
    )
import Graphics.Vty
    ( DisplayRegion
    , Attr
    , Image
    , Key
    , Modifier
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , FocusGroup
    , (<~)
    , (<~~)
    , getFocusGroup
    , updateWidgetState_
    , setPhysicalPosition
    , newWidget
    , updateWidget
    , render
    , handleKeyEvent
    , getState
    )

-- Ultimately we'd want support for "stacks" to provide things like
-- overlaid dialogs, but for now we'll just implement a "collection"
-- type which allows is to have a collection of widgets and switch
-- between them.

data CollectionError = EmptyCollection
                     | BadCollectionIndex Int
                       deriving (Show, Typeable)

instance Exception CollectionError

data Entry = forall a. Entry (Widget a)

data Collection =
    Collection { entries :: [Entry]
               , currentEntryNum :: Int
               }

renderEntry :: (MonadIO m) => Entry -> DisplayRegion -> Maybe Attr -> m Image
renderEntry (Entry w) = render w

positionEntry :: Entry -> DisplayRegion -> IO ()
positionEntry (Entry w) = setPhysicalPosition w

entryHandleKeyEvent :: (MonadIO m) => Entry -> Key -> [Modifier] -> m Bool
entryHandleKeyEvent (Entry w) k mods = handleKeyEvent w k mods

entryFocusGroup :: Entry -> IO (Maybe (Widget FocusGroup))
entryFocusGroup (Entry w) = getFocusGroup w

newCollection :: (MonadIO m) => m (Widget Collection)
newCollection = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Collection { entries = []
                             , currentEntryNum = -1
                             }
        -- XXX technically this should defer to whichever entry is
        -- current!
        , getGrowHorizontal = return True
        , getGrowVertical = return True

        , focusGroup =
            \this -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return Nothing
                i -> do
                       let e = entries st !! i
                       entryFocusGroup e

        , keyEventHandler =
            \this key mods -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return False
                i -> do
                       let e = entries st !! i
                       entryHandleKeyEvent e key mods

        , draw = \this size mAttr -> do
                   st <- getState this
                   case currentEntryNum st of
                     (-1) -> throw EmptyCollection
                     i -> do
                       let e = entries st !! i
                       renderEntry e size mAttr

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              st <- getState this
              case currentEntryNum st of
                (-1) -> throw EmptyCollection
                i -> do
                  let e = entries st !! i
                  positionEntry e pos
        }

addToCollection :: (MonadIO m) => Widget Collection -> Widget a -> m (m ())
addToCollection cRef wRef = do
  i <- (length . entries) <~~ cRef
  updateWidgetState_ cRef $ \st ->
      st { entries = (entries st) ++ [Entry wRef]
         , currentEntryNum = if currentEntryNum st == -1
                             then 0
                             else currentEntryNum st
         }
  return $ setCurrent cRef i

setCurrent :: (MonadIO m) => Widget Collection -> Int -> m ()
setCurrent cRef i = do
  st <- state <~ cRef
  if i < length (entries st) && i >= 0 then
      updateWidgetState_ cRef $ \s -> s { currentEntryNum = i } else
      throw $ BadCollectionIndex i
