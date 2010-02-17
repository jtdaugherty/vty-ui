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
    , selectedIndex
    , scrollTopIndex
    , scrollWindowSize
    , getVisibleItems
    )
where

import Graphics.Vty ( Attr, DisplayRegion )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Orientation(..)
    , Render
    )
import Graphics.Vty.Widgets.Rendering
    ( renderMany
    )
import Graphics.Vty.Widgets.Base
    ( hFill
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )

-- |A list item. Each item contains an arbitrary internal identifier
-- @a@ and a widget @b@ representing it.
type ListItem a = (a, Widget)

-- |The list widget type.  Lists are parameterized over the /internal/
-- /identifier type/ @a@, the type of internal identifiers used to
-- refer to the visible representations of the list contents, and the
-- /widget type/ @b@, the type of widgets used to represent the list
-- visually.
data List a = List { normalAttr :: Attr
                   , selectedAttr :: Attr
                   , selectedIndex :: Int
                   -- ^The currently selected list index.
                   , scrollTopIndex :: Int
                   -- ^The start index of the window of visible list
                   -- items.
                   , scrollWindowSize :: Int
                   -- ^The size of the window of visible list items.
                   , listItems :: [ListItem a]
                   -- ^The items in the list.
                   }

-- |Create a new list.  Emtpy lists and empty scrolling windows are
-- not allowed.
mkList :: Attr -- ^The attribute of normal, non-selected items
       -> Attr -- ^The attribute of the selected item
       -> Int -- ^The scrolling window size, i.e., the number of items
              -- which should be visible to the user at any given time
       -> [ListItem a] -- ^The list items
       -> List a
mkList _ _ _ [] = error "Lists cannot be empty"
mkList normAttr selAttr swSize contents
    | swSize <= 0 = error "Scrolling window size must be > 0"
    | otherwise = List normAttr selAttr 0 0 swSize contents

listWidget :: List a -> Widget
listWidget list = Widget {
                    growHorizontal = False
                  , growVertical = False
                  , withAttribute = \att -> listWidget list { normalAttr = att }
                  , primaryAttribute = normalAttr list
                  , render = renderListWidget list
                  }

renderListWidget :: List a -> DisplayRegion -> Render
renderListWidget list s =
    renderMany Vertical ws
        where
          ws = map (\w -> render w s) (visible ++ filler)
          visible = map highlight items
          items = map (\((_, w), sel) -> (w, sel)) $ getVisibleItems list
          filler = replicate (scrollWindowSize list - length visible)
                   (hFill (normalAttr list) ' ' 1)
          highlight (w, selected) = let att = if selected
                                              then selectedAttr
                                              else normalAttr
                                    in withAttribute w (att list)

-- |A convenience function to create a new list using 'String's as the
-- internal identifiers and 'Text' widgets to represent those strings.
mkSimpleList :: Attr -- ^The attribute of normal, non-selected items
             -> Attr -- ^The attribute of the selected item
             -> Int -- ^The scrolling window size, i.e., the number of
                    -- items which should be visible to the user at
                    -- any given time
             -> [String] -- ^The list items
             -> List String
mkSimpleList normAttr selAttr swSize labels =
    mkList normAttr selAttr swSize widgets
    where
      widgets = map (\l -> (l, simpleText normAttr l)) labels

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
-- |Get the currently selected list item.
getSelected :: List a -> ListItem a
getSelected list = (listItems list) !! (selectedIndex list)

-- |Set the window size of the list.  This automatically adjusts the
-- window position to keep the selected item visible.
resize :: Int -> List a -> List a
resize newSize list
    | newSize == 0 = error "Cannot resize list window to zero"
    -- Do nothing if the window size isn't changing.
    | newSize == scrollWindowSize list = list
    -- If the new window size is larger, just set it.
    | newSize > scrollWindowSize list = list { scrollWindowSize = newSize }
    -- Otherwise it's smaller, so we need to look at which item is
    -- selected and decide whether to change the scrollTopIndex.
    | otherwise = list { scrollWindowSize = newSize
                       , selectedIndex = newSelected
                       }
    where
      newBottomPosition = scrollTopIndex list + newSize - 1
      current = selectedIndex list
      newSelected = if current > newBottomPosition
                    then newBottomPosition
                    else current

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
scrollBy :: Int -> List a -> List a
scrollBy amount list =
    list { scrollTopIndex = adjustedTop
         , selectedIndex = newSelected }
        where
          sel = selectedIndex list
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

-- |Scroll a list down by one position.
scrollDown :: List a -> List a
scrollDown = scrollBy 1

-- |Scroll a list up by one position.
scrollUp :: List a -> List a
scrollUp = scrollBy (-1)

-- |Scroll a list down by one page from the current cursor position.
pageDown :: List a -> List a
pageDown list = scrollBy (scrollWindowSize list) list

-- |Scroll a list up by one page from the current cursor position.
pageUp :: List a -> List a
pageUp list = scrollBy (-1 * scrollWindowSize list) list

-- |Given a 'List', return the items that are currently visible
-- according to the state of the list.  Returns the visible items and
-- flags indicating whether each is selected.
getVisibleItems :: List a -> [(ListItem a, Bool)]
getVisibleItems list =
    let start = scrollTopIndex list
        stop = scrollTopIndex list + scrollWindowSize list
        adjustedStop = (min stop $ length $ listItems list) - 1
    in [ (listItems list !! i, i == selectedIndex list)
             | i <- [start..adjustedStop] ]