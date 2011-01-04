-- |This module provides a 'List' widget for rendering a list of
-- arbitrary widgets.  A 'List' has the following features:
--
-- * A style for the list elements
--
-- * A styled cursor indicating which element is selected
--
-- * A /window size/ indicating how many elements should be visible to
--   the user
--
-- * An internal pointer to the start of the visible window, which
--   automatically shifts as the list is scrolled
module Graphics.Vty.Widgets.List
    ( List
    , ListItem
    -- ** List creation
    , mkList
    , mkSimpleList
    , listWidget
    -- ** List manipulation
    , scrollBy
    , scrollUp
    , scrollDown
    , pageUp
    , pageDown
    , resize
    -- ** List inspection
    , listItems
    , getSelected
    , scrollTopIndex
    , scrollWindowSize
    , getVisibleItems
    )
where

import Control.Monad
    ( forM
    , when
    )
import Control.Monad.Trans
    ( MonadIO
    )
import Control.Monad
    ( replicateM
    )
import Graphics.Vty
    ( Attr
    , DisplayRegion
    , Image
    , Key(..)
    , vert_cat
    )
import Graphics.Vty.Widgets.Rendering
    ( WidgetImpl(..)
    , Widget
    , (<~)
    , (<~~)
    , render
    , newWidget
    , updateWidget
    , updateWidgetState_
    , getState
    )
import Graphics.Vty.Widgets.Base
    ( hFill
    )
import Graphics.Vty.Widgets.Text
    ( FormattedText
    , simpleText
    )

-- |A list item. Each item contains an arbitrary internal identifier
-- @a@ and a 'Widget' representing it.
type ListItem a b = (a, Widget b)

-- |The list widget type.  Lists are parameterized over the /internal/
-- /identifier type/ @a@, the type of internal identifiers used to
-- refer to the visible representations of the list contents, and the
-- /widget type/ @b@, the type of widgets used to represent the list
-- visually.
data List a b = List { normalAttr :: Attr
                     , selectedAttr :: Attr
                     , selectedIndex :: Int
                     -- ^The currently selected list index.
                     , scrollTopIndex :: Int
                     -- ^The start index of the window of visible list
                     -- items.
                     , scrollWindowSize :: Int
                     -- ^The size of the window of visible list items.
                     , listItems :: [ListItem a b]
                     -- ^The items in the list.
                     }

-- |Create a new list.  Emtpy lists and empty scrolling windows are
-- not allowed.
mkList :: Attr -- ^The attribute of normal, non-selected items
       -> Attr -- ^The attribute of the selected item
       -> Int -- ^The scrolling window size, i.e., the number of items
              -- which should be visible to the user at any given time
       -> [ListItem a b] -- ^The list items
       -> List a b
mkList _ _ _ [] = error "Lists cannot be empty"
mkList normAttr selAttr swSize contents
    | swSize <= 0 = error "Scrolling window size must be > 0"
    | otherwise = List normAttr selAttr 0 0 swSize contents

listWidget :: List a b -> IO (Widget (List a b))
listWidget list = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = list
        , getGrowHorizontal = return False
        , getGrowVertical = return False

        -- XXX it might be crazy, but we could even pass events we
        -- don't handle onto the currently selected widget!
        , keyEventHandler =
            \this k -> do
              v <- listKeyEvent this k
              return v

        , draw = \this sz mAttr -> do
            listData <- getState this
            renderListWidget listData sz mAttr
        }

listKeyEvent :: Widget (List a b) -> Key -> IO Bool
listKeyEvent w KUp = scrollUp w >> return True
listKeyEvent w KDown = scrollDown w >> return True
listKeyEvent w KPageUp = pageUp w >> return True
listKeyEvent w KPageDown = pageDown w >> return True
listKeyEvent _ _ = return False

renderListWidget :: List a b -> DisplayRegion -> Maybe Attr -> IO Image
renderListWidget list s mAttr = do
  let items = map (\((_, w), sel) -> (w, sel)) $ getVisibleItems_ list
      visible_render (w, sel) = render w s att
          where
            att = if sel then (Just $ selectedAttr list) else mAttr

  filler_ws <- replicateM (scrollWindowSize list - length items)
               (hFill (normalAttr list) ' ' 1)
  filler_imgs <- mapM (\w -> render w s mAttr) filler_ws
  visible_imgs <- mapM visible_render items

  return $ vert_cat (visible_imgs ++ filler_imgs)

-- |A convenience function to create a new list using 'String's as the
-- internal identifiers and 'Text' widgets to represent those strings.
mkSimpleList :: (MonadIO m) =>
                Attr -- ^The attribute of normal, non-selected items
             -> Attr -- ^The attribute of the selected item
             -> Int -- ^The scrolling window size, i.e., the number of
                    -- items which should be visible to the user at
                    -- any given time
             -> [String] -- ^The list items
             -> m (List String FormattedText)
mkSimpleList normAttr selAttr swSize labels = do
  pairs <- forM labels $ \l -> do
             w <- simpleText normAttr l
             return (l, w)
  return $ mkList normAttr selAttr swSize pairs

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
-- |Get the currently selected list item.
getSelected :: (MonadIO m) => Widget (List a b) -> m (Int, ListItem a b)
getSelected wRef = do
  list <- state <~ wRef
  return $ (selectedIndex list, (listItems list) !! (selectedIndex list))

-- |Set the window size of the list.  This automatically adjusts the
-- window position to keep the selected item visible.
resize :: (MonadIO m) => Int -> Widget (List a b) -> m ()
resize newSize wRef = do
  when (newSize == 0) $ error "Cannot resize list window to zero"

  size <- (scrollWindowSize . state) <~ wRef

  case compare newSize size of
    EQ -> return () -- Do nothing if the window size isn't changing.
    GT -> updateWidgetState_ wRef $ \list -> list { scrollWindowSize = newSize }
    -- Otherwise it's smaller, so we need to look at which item is
    -- selected and decide whether to change the scrollTopIndex.
    LT -> do
      list <- state <~ wRef
      let newBottomPosition = scrollTopIndex list + newSize - 1
          current = selectedIndex list
          newSelected = if current > newBottomPosition
                        then newBottomPosition
                        else current
      updateWidgetState_ wRef $ const $ list { scrollWindowSize = newSize
                                             , selectedIndex = newSelected
                                             }

-- |Scroll a list up or down by the specified number of positions and
-- return the new scrolled list.  Scrolling by a positive amount
-- scrolls downward and scrolling by a negative amount scrolls upward.
-- This automatically takes care of managing internal list state:
--
-- * Moves the cursor by the specified amount and clamps the cursor
--   position to the beginning or the end of the list where
--   appropriate
--
-- * Moves the scrolling window position if necessary (i.e., if the
--   cursor moves to an item not currently in view)
scrollBy :: (MonadIO m) => Int -> Widget (List a b) -> m ()
scrollBy amount wRef = updateWidgetState_ wRef $ scrollBy' amount

-- Pure interface; should be used internally to the widget.
scrollBy' :: Int -> List a b -> List a b
scrollBy' amount list =
  let sel = selectedIndex list
      lastPos = (length $ listItems list) - 1
      validPositions = [0..lastPos]
      newPosition = sel + amount

      newSelected = if newPosition `elem` validPositions
                    then newPosition
                    else if newPosition > lastPos
                         then lastPos
                         else 0

      bottomPosition = scrollTopIndex list + scrollWindowSize list - 1
      topPosition = scrollTopIndex list
      windowPositions = [topPosition..bottomPosition]

      adjustedTop = if newPosition `elem` windowPositions
                    then topPosition
                    else if newSelected >= bottomPosition
                         then newSelected - scrollWindowSize list + 1
                         else newSelected

  in list { scrollTopIndex = adjustedTop
          , selectedIndex = newSelected }

-- |Scroll a list down by one position.
scrollDown :: (MonadIO m) => Widget (List a b) -> m ()
scrollDown = scrollBy 1

-- |Scroll a list up by one position.
scrollUp :: (MonadIO m) => Widget (List a b) -> m ()
scrollUp = scrollBy (-1)

-- |Scroll a list down by one page from the current cursor position.
pageDown :: (MonadIO m) => Widget (List a b) -> m ()
pageDown wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy amt wRef

-- |Scroll a list up by one page from the current cursor position.
pageUp :: (MonadIO m) => Widget (List a b) -> m ()
pageUp wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy (-1 * amt) wRef

-- |Given a 'List', return the items that are currently visible
-- according to the state of the list.  Returns the visible items and
-- flags indicating whether each is selected.
getVisibleItems :: (MonadIO m) => Widget (List a b) -> m [(ListItem a b, Bool)]
getVisibleItems wRef = do
  list <- state <~ wRef
  return $ getVisibleItems_ list

getVisibleItems_ :: List a b -> [(ListItem a b, Bool)]
getVisibleItems_ list =
    let start = scrollTopIndex list
        stop = scrollTopIndex list + scrollWindowSize list
        adjustedStop = (min stop $ length $ listItems list) - 1
    in [ (listItems list !! i, i == selectedIndex list)
             | i <- [start..adjustedStop] ]