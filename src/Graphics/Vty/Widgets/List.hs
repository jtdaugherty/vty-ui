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
    , addToList
    -- ** List manipulation
    , scrollBy
    , scrollUp
    , scrollDown
    , pageUp
    , pageDown
    , resize
    , onSelectionChange
    , onItemAdded
    -- ** List inspection
    , listItems
    , getSelected
    , scrollTopIndex
    , scrollWindowSize
    , getVisibleItems
    )
where

import Data.Maybe
    ( isJust
    , fromJust
    )
import Control.Monad
    ( when
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
    ( Attr
    , DisplayRegion(..)
    , Image
    , Key(..)
    , Modifier
    , vert_cat
    , image_height
    , char_fill
    , def_attr
    )
import Graphics.Vty.Widgets.Core
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
                     , selectionChangeHandler :: Widget (List a b) -> IO ()
                     , itemAddHandler :: Widget (List a b) -> Int -> a -> Widget b -> IO ()
                     , itemHeight :: Int
                     , itemConstructor :: a -> IO (Widget b)
                     -- ^Function to construct new items
                     }

-- |Create a new list.  Emtpy lists and empty scrolling windows are
-- not allowed.
mkList :: Attr -- ^The attribute of normal, non-selected items
       -> Attr -- ^The attribute of the selected item
       -> (a -> IO (Widget b)) -- ^Constructor for new item widgets
       -> List a b
mkList normAttr selAttr f =
    List { normalAttr = normAttr
         , selectedAttr = selAttr
         , selectedIndex = -1
         , scrollTopIndex = 0
         , scrollWindowSize = 0
         , listItems = []
         , selectionChangeHandler = const $ return ()
         , itemAddHandler = \_ _ _ _ -> return ()
         , itemHeight = 0
         , itemConstructor = f
         }

addToList :: (MonadIO m) => Widget (List a b) -> a -> m ()
addToList list key = do
  numItems <- (length . listItems) <~~ list

  makeWidget <- itemConstructor <~~ list
  w <- liftIO $ makeWidget key

  h <- case numItems of
         0 -> do
           -- We're adding the first element to the list, so we need
           -- to compute the item height based on this widget.  We
           -- just render it in an unreasonably large space (since,
           -- really, list items should never be THAT big) and measure
           -- the result, assuming that all list widgets will have the
           -- same size.  If you violate this, you'll have interesting
           -- results!
           img <- render w (DisplayRegion 100 100) Nothing
           return $ fromEnum $ image_height img
         _ -> itemHeight <~~ list

  updateWidgetState_ list $ \s -> s { itemHeight = h
                                    , listItems = listItems s ++ [(key, w)]
                                    , selectedIndex = if numItems == 0
                                                      then 0
                                                      else selectedIndex s
                                    }

  notifyItemAddHandler list (numItems + 1) key w

  when (numItems == 0) $
       notifySelectionHanlder list

onSelectionChange :: (MonadIO m) => Widget (List a b) -> (Widget (List a b) -> IO ()) -> m ()
onSelectionChange wRef handler = do
  oldHandler <- selectionChangeHandler <~~ wRef

  let combinedHandler =
          \w -> do
            oldHandler w
            handler w

  updateWidgetState_ wRef $ \s -> s { selectionChangeHandler = combinedHandler }

onItemAdded :: (MonadIO m) => Widget (List a b)
            -> (Widget (List a b) -> Int -> a -> Widget b -> IO ()) -> m ()
onItemAdded wRef handler = do
  oldHandler <- itemAddHandler <~~ wRef

  let combinedHandler =
          \w pos k iw -> do
            oldHandler w pos k iw
            handler w pos k iw

  updateWidgetState_ wRef $ \s -> s { itemAddHandler = combinedHandler }

listWidget :: (MonadIO m) => List a b -> m (Widget (List a b))
listWidget list = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = list
        , getGrowHorizontal = return False
        , getGrowVertical = return True

        -- XXX it might be crazy, but we could even pass events we
        -- don't handle onto the currently selected widget!
        , keyEventHandler = listKeyEvent

        , draw =
            \this sz mAttr -> do
              h <- itemHeight <~~ this

              -- Resize the list based on the available space and the
              -- height of each item.
              when (h > 0) $
                   resize ((fromEnum $ region_height sz) `div` h) this

              listData <- getState this
              renderListWidget listData sz mAttr

        -- XXX!!! define setPosition to set position of visible
        -- widgets in list
        }

listKeyEvent :: Widget (List a b) -> Key -> [Modifier] -> IO Bool
listKeyEvent w KUp _ = scrollUp w >> return True
listKeyEvent w KDown _ = scrollDown w >> return True
listKeyEvent w KPageUp _ = pageUp w >> return True
listKeyEvent w KPageDown _ = pageDown w >> return True
listKeyEvent _ _ _ = return False

renderListWidget :: List a b -> DisplayRegion -> Maybe Attr -> IO Image
renderListWidget list s mAttr = do
  let items = map (\((_, w), sel) -> (w, sel)) $ getVisibleItems_ list

      renderVisible [] = return []
      renderVisible ((w, sel):ws) = do
        let att = if sel
                  then (Just $ selectedAttr list)
                  else if isJust mAttr
                       then mAttr
                       else (Just $ normalAttr list)
        img <- render w s att
        imgs <- renderVisible ws
        return (img:imgs)

  -- XXX this is probably incorrect for widgets with height > 1
  let filler = char_fill attr ' ' (region_width s) fill_height
      fill_height = if scrollWindowSize list == 0
                    then region_height s
                    else toEnum $ scrollWindowSize list - length items
      attr = if isJust mAttr then fromJust mAttr else normalAttr list

  visible_imgs <- renderVisible items

  return $ vert_cat (visible_imgs ++ [filler])

-- |A convenience function to create a new list using 'String's as the
-- internal identifiers and 'Text' widgets to represent those strings.
mkSimpleList :: (MonadIO m) =>
                Attr -- ^The attribute of normal, non-selected items
             -> Attr -- ^The attribute of the selected item
             -> [String] -- ^The list items
             -> m (Widget (List String FormattedText))
mkSimpleList normAttr selAttr labels = do
  list <- listWidget $ mkList normAttr selAttr (simpleText def_attr)
  mapM_ (addToList list) labels
  return list

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
-- |Get the currently selected list item.
getSelected :: (MonadIO m) => Widget (List a b) -> m (Maybe (Int, ListItem a b))
getSelected wRef = do
  list <- state <~ wRef
  case selectedIndex list of
    (-1) -> return Nothing
    i -> return $ Just (i, (listItems list) !! i)

-- |Set the window size of the list.  This automatically adjusts the
-- window position to keep the selected item visible.
resize :: (MonadIO m) => Int -> Widget (List a b) -> m ()
resize newSize wRef = do
  when (newSize == 0) $ error "Cannot resize list window to zero"

  size <- (scrollWindowSize . state) <~ wRef

  case compare newSize size of
    EQ -> return () -- Do nothing if the window size isn't changing.
    GT -> updateWidgetState_ wRef $ \list ->
          list { scrollWindowSize = newSize
               , scrollTopIndex = max 0 (scrollTopIndex list - (newSize - scrollWindowSize list))
               }
    -- Otherwise it's smaller, so we need to look at which item is
    -- selected and decide whether to change the scrollTopIndex.
    LT -> do
      list <- state <~ wRef

      -- If the currently selected item would be out of view in the
      -- new size, then we need to move the display top down to keep
      -- it visible.
      let newBottomPosition = scrollTopIndex list + newSize - 1
          current = selectedIndex list
          newScrollTopIndex = if current > newBottomPosition
                              then current - newSize + 1
                              else scrollTopIndex list

      updateWidgetState_ wRef $ const $ list { scrollWindowSize = newSize
                                             , scrollTopIndex = newScrollTopIndex
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

  in if scrollWindowSize list == 0
     then list
     else list { scrollTopIndex = adjustedTop
               , selectedIndex = newSelected }

notifySelectionHanlder :: (MonadIO m) => Widget (List a b) -> m ()
notifySelectionHanlder wRef = do
  h <- selectionChangeHandler <~~ wRef
  liftIO $ h wRef

notifyItemAddHandler :: (MonadIO m) => Widget (List a b) -> Int -> a -> Widget b -> m ()
notifyItemAddHandler wRef pos k w = do
  h <- itemAddHandler <~~ wRef
  liftIO $ h wRef pos k w

-- |Scroll a list down by one position.
scrollDown :: (MonadIO m) => Widget (List a b) -> m ()
scrollDown wRef = scrollBy 1 wRef >> notifySelectionHanlder wRef

-- |Scroll a list up by one position.
scrollUp :: (MonadIO m) => Widget (List a b) -> m ()
scrollUp wRef = scrollBy (-1) wRef >> notifySelectionHanlder wRef

-- |Scroll a list down by one page from the current cursor position.
pageDown :: (MonadIO m) => Widget (List a b) -> m ()
pageDown wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy amt wRef
  notifySelectionHanlder wRef

-- |Scroll a list up by one page from the current cursor position.
pageUp :: (MonadIO m) => Widget (List a b) -> m ()
pageUp wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy (-1 * amt) wRef
  notifySelectionHanlder wRef

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