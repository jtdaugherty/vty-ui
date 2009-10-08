{-# LANGUAGE ExistentialQuantification #-}
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
    , SimpleList
    , ListItem
    -- ** List creation
    , mkList
    , mkSimpleList
    -- ** List manipulation
    , scrollBy
    , scrollUp
    , scrollDown
    , pageUp
    , pageDown
    -- ** List inspection
    , listItems
    , getSelected
    , selectedIndex
    , scrollTopIndex
    , scrollWindowSize
    , getVisibleItems
    )
where

import Graphics.Vty ( Attr, vert_cat )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , Text
    , text
    , anyWidget
    , hFill
    )

-- |A list item. Each item contains an arbitrary internal identifier
-- @a@ and a widget @b@ representing it.
type ListItem a b = (a, b)

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

type SimpleList = List String Text

-- |Create a new list.  Emtpy lists are not allowed.
mkList :: (Widget b) =>
          Attr -- ^The attribute of normal, non-selected items
       -> Attr -- ^The attribute of the selected item
       -> Int -- ^The scrolling window size, i.e., the number of items
              -- which should be visible to the user at any given time
       -> [ListItem a b] -- ^The list items
       -> List a b
mkList _ _ _ [] = error "Lists cannot be empty"
mkList normAttr selAttr swSize contents = List normAttr selAttr 0 0 swSize contents

-- |A convenience function to create a new list using 'String's as the
-- internal identifiers and 'Text' widgets to represent those strings.
mkSimpleList :: Attr -- ^The attribute of normal, non-selected items
             -> Attr -- ^The attribute of the selected item
             -> Int -- ^The scrolling window size, i.e., the number of
                    -- items which should be visible to the user at
                    -- any given time
             -> [String] -- ^The list items
             -> SimpleList
mkSimpleList normAttr selAttr swSize labels =
    mkList normAttr selAttr swSize widgets
    where
      widgets = map (\l -> (l, text normAttr l)) labels

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
-- |Get the currently selected list item.
getSelected :: List a b -> ListItem a b
getSelected list = (listItems list) !! (selectedIndex list)

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
scrollBy :: Int -> List a b -> List a b
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
scrollDown :: List a b -> List a b
scrollDown = scrollBy 1

-- |Scroll a list up by one position.
scrollUp :: List a b -> List a b
scrollUp = scrollBy (-1)

-- |Scroll a list down by one page from the current cursor position.
pageDown :: List a b -> List a b
pageDown list = scrollBy (scrollWindowSize list) list

-- |Scroll a list up by one page from the current cursor position.
pageUp :: List a b -> List a b
pageUp list = scrollBy (-1 * scrollWindowSize list) list

-- |Given a 'List', return the items that are currently visible
-- according to the state of the list.  Returns the visible items and
-- flags indicating whether each is selected.
getVisibleItems :: List a b -> [(ListItem a b, Bool)]
getVisibleItems list =
    let start = scrollTopIndex list
        stop = scrollTopIndex list + scrollWindowSize list
        adjustedStop = (min stop $ length $ listItems list) - 1
    in [ (listItems list !! i, i == selectedIndex list)
             | i <- [start..adjustedStop] ]

instance (Widget b) => Widget (List a b) where
    growHorizontal _ = False
    growVertical _ = False

    withAttribute w att = w { normalAttr = att }

    primaryAttribute = normalAttr

    render s list =
        vert_cat images
            where
              images = map (render s) (visible ++ filler)
              visible = map highlight items
              items = map (\((_, w), sel) -> (w, sel)) $ getVisibleItems list
              filler = replicate (scrollWindowSize list - length visible)
                       (anyWidget $ hFill (normalAttr list) ' ' 1)
              highlight (w, selected) = let att = if selected
                                                  then selectedAttr
                                                  else normalAttr
                                        in anyWidget $ withAttribute w (att list)