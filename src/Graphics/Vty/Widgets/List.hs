{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
-- |This module provides a 'List' widget for rendering a list of
-- arbitrary widgets.  A 'List' shows a number of elements and
-- highlights the currently-selected widget.  It supports key events
-- to navigate the list and will automatically scroll based on the
-- space available to the list along with the size of the widgets in
-- the list.
module Graphics.Vty.Widgets.List
    ( List
    , ListItem
    , ListError(..)
    , NewItemEvent(..)
    , RemoveItemEvent(..)
    , SelectionEvent(..)
    , ActivateItemEvent(..)
    -- ** List creation
    , newStringList
    , newList
    , addToList
    , insertIntoList
    , removeFromList
    -- ** List manipulation
    , scrollBy
    , scrollUp
    , scrollDown
    , pageUp
    , pageDown
    , onSelectionChange
    , onItemAdded
    , onItemRemoved
    , onItemActivated
    , activateCurrentItem
    , clearList
    , setSelected
    -- ** List inspection
    , getListSize
    , getSelected
    , getListItem
    )
where

import Data.Typeable
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data ListError = BadItemIndex Int
               -- ^The specified position could not be used to remove
               -- an item from the list.
               | ResizeError
               | BadListWidgetSizePolicy
               -- ^The type of widgets added to the list grow
               -- vertically, which is not permitted.
                 deriving (Show, Typeable)

instance Exception ListError

-- |A list item. Each item contains an arbitrary internal value @a@
-- and a 'Widget' representing it.
type ListItem a b = (a, Widget b)

data SelectionEvent a b = SelectionOn Int a (Widget b)
                        -- ^An item at the specified position with the
                        -- specified internal value and widget was
                        -- selected.
                        | SelectionOff
                          -- ^No item was selected, which means the
                          -- list is empty.

-- |A new item was added to the list at the specified position with
-- the specified value and widget.
data NewItemEvent a b = NewItemEvent Int a (Widget b)

-- |An item was removed from the list at the specified position with
-- the specified value and widget.
data RemoveItemEvent a b = RemoveItemEvent Int a (Widget b)

-- |An item in the list was activated at the specified position with
-- the specified value and widget.
data ActivateItemEvent a b = ActivateItemEvent Int a (Widget b)

-- |The list widget type.  Lists are parameterized over the /internal/
-- /value type/ @a@, the type of internal values used to refer to the
-- visible representations of the list contents, and the /widget type/
-- @b@, the type of widgets used to represent the list visually.
data List a b = List { selectedUnfocusedAttr :: Attr
                     , selectedIndex :: Int
                     -- ^The currently selected list index.
                     , scrollTopIndex :: Int
                     -- ^The start index of the window of visible list
                     -- items.
                     , scrollWindowSize :: Int
                     -- ^The size of the window of visible list items.
                     , listItems :: [ListItem a b]
                     -- ^The items in the list.
                     , selectionChangeHandlers :: Handlers (SelectionEvent a b)
                     , itemAddHandlers :: Handlers (NewItemEvent a b)
                     , itemRemoveHandlers :: Handlers (RemoveItemEvent a b)
                     , itemActivateHandlers :: Handlers (ActivateItemEvent a b)
                     , itemHeight :: Int
                     , itemConstructor :: a -> IO (Widget b)
                     -- ^Function to construct new items
                     }

instance Show (List a b) where
    show lst = concat [ "List { "
                      , "selectedUnfocusedAttr = ", show $ selectedUnfocusedAttr lst
                      , ", selectedIndex = ", show $ selectedIndex lst
                      , ", scrollTopIndex = ", show $ scrollTopIndex lst
                      , ", scrollWindowSize = ", show $ scrollWindowSize lst
                      , ", listItems = <", show $ length $ listItems lst, " items>"
                      , ", itemHeight = ", show $ itemHeight lst
                      , " }"
                      ]

newListData :: 
               Attr -- ^The attribute of the selected item
            -> (a -> IO (Widget b)) -- ^Constructor for new item widgets
            -> IO (List a b)
newListData selAttr f = do
  schs <- newHandlers
  iahs <- newHandlers
  irhs <- newHandlers
  iacths <- newHandlers

  return $ List { selectedUnfocusedAttr = selAttr
                , selectedIndex = -1
                , scrollTopIndex = 0
                , scrollWindowSize = 0
                , listItems = []
                , selectionChangeHandlers = schs
                , itemAddHandlers = iahs
                , itemRemoveHandlers = irhs
                , itemActivateHandlers = iacths
                , itemHeight = 0
                , itemConstructor = f
                }

-- |Get the length of the list in elements.
getListSize :: Widget (List a b) -> IO Int
getListSize = ((length . listItems) <~~)

-- |Remove an element from the list at the specified position.  May
-- throw 'BadItemIndex'.
removeFromList :: Widget (List a b) -> Int -> IO (ListItem a b)
removeFromList list pos = do
  st <- getState list
  foc <- focused <~ list

  let numItems = length $ listItems st
      oldScr = scrollTopIndex st

  when (pos < 0 || pos >= numItems) $
       throw $ BadItemIndex pos

  -- Get the item from the list.
  let (label, w) = listItems st !! pos
      sel = selectedIndex st

      newScrollTop = if pos <= oldScr
                     then if oldScr == 0
                          then oldScr
                          else oldScr - 1
                     else oldScr

      -- If that item is currently selected, select a different item.
      newSelectedIndex = if pos > sel
                         then sel
                         else if pos < sel
                              then if sel == 0
                                   then 0
                                   else sel - 1
                              else if sel == 0
                                   then if numItems == 1
                                        then (-1)
                                        else 0
                                   else if sel == numItems - 1
                                        then sel - 1
                                        else sel

  updateWidgetState list $ \s -> s { selectedIndex = newSelectedIndex
                                   , listItems = take pos (listItems st) ++
                                                 drop (pos + 1) (listItems st)
                                   , scrollTopIndex = newScrollTop
                                   }

  when foc $ do
    -- Unfocus the item we are about to remove if it's currently
    -- selected
    when (pos == sel) $ do
               unfocus w
               -- Focus the newly-selected item, if any
               cur <- getSelected list
               case cur of
                 Nothing -> return ()
                 Just (_, (_, w')) -> focus w'

  -- Notify the removal handler.
  notifyItemRemoveHandler list pos label w

  -- Notify the selection handler, but only if the position we deleted
  -- from is the selected position; that means the selection changed.
  --
  -- XXX this should probably be ==, not <=.  Do some testing.
  when (pos <= selectedIndex st) $
       notifySelectionHandler list

  -- Return the removed item.
  return (label, w)

-- |Add an item to the list.  Its widget will be constructed from the
-- specified internal value using the widget constructor passed to
-- 'newList'.
addToList :: (Show b) => Widget (List a b) -> a -> IO (ListItem a b)
addToList list key = do
  numItems <- (length . listItems) <~~ list
  insertIntoList list key numItems

insertIntoList :: (Show b) => Widget (List a b) -> a -> Int -> IO (ListItem a b)
insertIntoList list key pos = do
  numItems <- (length . listItems) <~~ list
  makeWidget <- itemConstructor <~~ list
  w <- liftIO $ makeWidget key

  v <- growVertical w
  when (v) $ throw BadListWidgetSizePolicy

  h <- case numItems of
         0 -> do
           -- We're adding the first element to the list, so we need
           -- to compute the item height based on this widget.  We
           -- just render it in an unreasonably large space (since,
           -- really, list items should never be THAT big) and measure
           -- the result, assuming that all list widgets will have the
           -- same size.  If you violate this, you'll have interesting
           -- results!
           img <- render w (DisplayRegion 100 100) defaultContext
           return $ fromEnum $ image_height img
         _ -> itemHeight <~~ list

  -- Calculate the new selected index.
  oldSel <- selectedIndex <~~ list
  oldScr <- scrollTopIndex <~~ list
  swSize <- scrollWindowSize <~~ list

  let newSelIndex = if numItems == 0
                    then 0
                    else if pos <= oldSel
                         then oldSel + 1
                         else oldSel
      newScrollTop = if pos <= oldSel
                     then if (oldSel - oldScr + 1) == swSize
                          then oldScr + 1
                          else oldScr
                     else oldScr

  updateWidgetState list $ \s -> s { itemHeight = h
                                   , listItems = inject pos (key, w) (listItems s)
                                   , selectedIndex = newSelIndex
                                   , scrollTopIndex = newScrollTop
                                   }

  notifyItemAddHandler list (numItems + 1) key w

  when (numItems == 0) $
       do
         foc <- focused <~ list
         when foc $ focus w

  when (oldSel /= newSelIndex) $ notifySelectionHandler list

  return (key, w)

-- |Register event handlers to be invoked when the list's selected
-- item changes.
onSelectionChange :: 
                     Widget (List a b)
                  -> (SelectionEvent a b -> IO ())
                  -> IO ()
onSelectionChange = addHandler (selectionChangeHandlers <~~)

-- |Register event handlers to be invoked when a new item is added to
-- the list.
onItemAdded :: Widget (List a b)
            -> (NewItemEvent a b -> IO ()) -> IO ()
onItemAdded = addHandler (itemAddHandlers <~~)

-- |Register event handlers to be invoked when an item is removed from
-- the list.
onItemRemoved :: Widget (List a b)
              -> (RemoveItemEvent a b -> IO ()) -> IO ()
onItemRemoved = addHandler (itemRemoveHandlers <~~)

-- |Register event handlers to be invoked when an item is activated,
-- which happens when the user presses Enter on a selected element
-- while the list has the focus.
onItemActivated :: Widget (List a b)
            -> (ActivateItemEvent a b -> IO ()) -> IO ()
onItemActivated = addHandler (itemActivateHandlers <~~)

-- |Clear the list, removing all elements.  Does not invoke any
-- handlers.
clearList :: Widget (List a b) -> IO ()
clearList w = do
  updateWidgetState w $ \l ->
      l { selectedIndex = (-1)
        , scrollTopIndex = 0
        , listItems = []
        }

-- |Create a new list using the specified attribute for the
-- currently-selected element when the list does NOT have focus.  Use
-- the specified constructor function to create widgets for new items
-- in the list.
newList :: (Show b) =>
           Attr -- ^The attribute of the selected item
        -> (a -> IO (Widget b)) -- ^Constructor for new item widgets
        -> IO (Widget (List a b))
newList selAttr f = do
  list <- newListData selAttr f
  wRef <- newWidget $ \w ->
      w { state = list
        , keyEventHandler = listKeyEvent

        , growVertical_ = const $ return True
        , growHorizontal_ = const $ return True

        , getCursorPosition_ =
            \this -> do
              sel <- getSelected this
              case sel of
                Nothing -> return Nothing
                Just (_, (_, e)) -> getCursorPosition e

        , render_ =
            \this sz ctx -> do
              -- Get the item height *before* a potential resize, then
              -- get the list state below, after the resize.
              h <- itemHeight <~~ this

              -- Resize the list based on the available space and the
              -- height of each item.
              when (h > 0) $
                   resize this (max 1 ((fromEnum $ region_height sz) `div` h))

              listData <- getState this
              foc <- focused <~ this

              renderListWidget foc listData sz ctx

        , setCurrentPosition_ =
            \this pos -> do
              ih <- itemHeight <~~ this
              items <- getVisibleItems this
              forM_ (zip [0..] items) $ \(i, ((_, iw), _)) ->
                  setCurrentPosition iw (pos `plusHeight` (toEnum $ i * ih))
        }

  wRef `onGainFocus` \_ ->
      do
        val <- getSelected wRef
        case val of
          Nothing -> return ()
          Just (_, (_, w)) -> focus w

  wRef `onLoseFocus` \_ ->
      do
        val <- getSelected wRef
        case val of
          Nothing -> return ()
          Just (_, (_, w)) -> unfocus w

  return wRef

listKeyEvent :: Widget (List a b) -> Key -> [Modifier] -> IO Bool
listKeyEvent w KUp _ = scrollUp w >> return True
listKeyEvent w KDown _ = scrollDown w >> return True
listKeyEvent w KPageUp _ = pageUp w >> return True
listKeyEvent w KPageDown _ = pageDown w >> return True
listKeyEvent w KEnter _ = activateCurrentItem w >> return True
listKeyEvent w k mods = do
  val <- getSelected w
  case val of
    Nothing -> return False
    Just (_, (_, e)) -> handleKeyEvent e k mods

renderListWidget :: (Show b) => Bool -> List a b -> DisplayRegion -> RenderContext -> IO Image
renderListWidget foc list s ctx = do
  let items = map (\((_, w), sel) -> (w, sel)) $ getVisibleItems_ list
      defaultAttr = mergeAttrs [ overrideAttr ctx
                               , normalAttr ctx
                               ]

      renderVisible [] = return []
      renderVisible ((w, sel):ws) = do
        let att = if sel
                  then if foc
                       then focusAttr ctx
                       else mergeAttrs [ selectedUnfocusedAttr list
                                       , defaultAttr
                                       ]
                  else defaultAttr
        img <- render w s $ ctx { overrideAttr = att }

        let actualHeight = min (region_height s) (toEnum $ itemHeight list)
            img' = img <|> char_fill att ' '
                   (region_width s - image_width img)
                   actualHeight
        imgs <- renderVisible ws
        return (img':imgs)

  let filler = char_fill defaultAttr ' ' (region_width s) fill_height
      fill_height = if scrollWindowSize list == 0
                    then region_height s
                    else toEnum $ ((scrollWindowSize list - length items) * itemHeight list)

  visible_imgs <- renderVisible items

  return $ vert_cat (visible_imgs ++ [filler])

-- |A convenience function to create a new list using 'String's as the
-- internal values and 'FormattedText' widgets to represent those
-- strings.
newStringList :: 
                 Attr -- ^The attribute of the selected item
              -> [String] -- ^The list items
              -> IO (Widget (List String FormattedText))
newStringList selAttr labels = do
  list <- newList selAttr plainText
  mapM_ (addToList list) labels
  return list

-- |Programmatically activate the currently-selected item in the list,
-- if any.
activateCurrentItem :: Widget (List a b) -> IO ()
activateCurrentItem wRef = do
  mSel <- getSelected wRef
  case mSel of
    Nothing -> return ()
    Just (pos, (val, w)) ->
        fireEvent wRef (itemActivateHandlers <~~) $ ActivateItemEvent pos val w

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
-- |Get the currently-selected list item.
getSelected :: Widget (List a b) -> IO (Maybe (Int, ListItem a b))
getSelected wRef = do
  list <- state <~ wRef
  case selectedIndex list of
    (-1) -> return Nothing
    i -> return $ Just (i, (listItems list) !! i)

-- |Get the list item at the specified position.
getListItem :: Widget (List a b) -> Int -> IO (Maybe (ListItem a b))
getListItem wRef pos = do
  list <- state <~ wRef
  case pos >= 0 && pos < (length $ listItems list) of
    False ->  return Nothing
    True -> return $ Just ((listItems list) !! pos)

-- |Set the currently-selected list index.
setSelected :: Widget (List a b) -> Int -> IO ()
setSelected wRef newPos = do
  list <- state <~ wRef
  case selectedIndex list of
    (-1) -> return ()
    curPos -> scrollBy wRef (newPos - curPos)

resize :: Widget (List a b) -> Int -> IO ()
resize wRef newSize = do
  when (newSize == 0) $ throw ResizeError

  size <- (scrollWindowSize . state) <~ wRef

  case compare newSize size of
    EQ -> return () -- Do nothing if the window size isn't changing.
    GT -> updateWidgetState wRef $ \list ->
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

      updateWidgetState wRef $ const $ list { scrollWindowSize = newSize
                                            , scrollTopIndex = newScrollTopIndex
                                            }

-- |Scroll a list up or down by the specified number of positions.
-- Scrolling by a positive amount scrolls downward and scrolling by a
-- negative amount scrolls upward.  This automatically takes care of
-- managing internal list state and invoking event handlers.
scrollBy :: Widget (List a b) -> Int -> IO ()
scrollBy wRef amount = do
  foc <- focused <~ wRef

  -- Unfocus the currently-selected item.
  old <- getSelected wRef
  case old of
    Nothing -> return ()
    Just (_, (_, w)) -> when foc $ unfocus w

  updateWidgetState wRef $ scrollBy' amount

  -- Focus the newly-selected item.
  new <- getSelected wRef
  case new of
    Nothing -> return ()
    Just (_, (_, w)) -> when foc $ focus w

  notifySelectionHandler wRef

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

      bottomPosition = min (scrollTopIndex list + scrollWindowSize list - 1)
                       ((length $ listItems list) - 1)
      topPosition = scrollTopIndex list
      windowPositions = [topPosition..bottomPosition]

      adjustedTop = if newSelected `elem` windowPositions
                    then topPosition
                    else if newSelected >= bottomPosition
                         then newSelected - scrollWindowSize list + 1
                         else newSelected

  in if scrollWindowSize list == 0
     then list
     else list { scrollTopIndex = adjustedTop
               , selectedIndex = newSelected }

notifySelectionHandler :: Widget (List a b) -> IO ()
notifySelectionHandler wRef = do
  sel <- getSelected wRef
  case sel of
    Nothing ->
        fireEvent wRef (selectionChangeHandlers <~~) SelectionOff
    Just (pos, (a, b)) ->
        fireEvent wRef (selectionChangeHandlers <~~) $ SelectionOn pos a b

notifyItemRemoveHandler :: Widget (List a b) -> Int -> a -> Widget b -> IO ()
notifyItemRemoveHandler wRef pos k w =
    fireEvent wRef (itemRemoveHandlers <~~) $ RemoveItemEvent pos k w

notifyItemAddHandler :: Widget (List a b) -> Int -> a -> Widget b -> IO ()
notifyItemAddHandler wRef pos k w =
    fireEvent wRef (itemAddHandlers <~~) $ NewItemEvent pos k w

-- |Scroll a list down by one position.
scrollDown :: Widget (List a b) -> IO ()
scrollDown wRef = scrollBy wRef 1

-- |Scroll a list up by one position.
scrollUp :: Widget (List a b) -> IO ()
scrollUp wRef = scrollBy wRef (-1)

-- |Scroll a list down by one page from the current cursor position.
pageDown :: Widget (List a b) -> IO ()
pageDown wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy wRef amt

-- |Scroll a list up by one page from the current cursor position.
pageUp :: Widget (List a b) -> IO ()
pageUp wRef = do
  amt <- scrollWindowSize <~~ wRef
  scrollBy wRef (-1 * amt)

getVisibleItems :: Widget (List a b) -> IO [(ListItem a b, Bool)]
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
