{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
-- |This module is the core of this library; it provides
-- infrastructure for creating new types of widgets and extending
-- their functionality.  This module provides various bits of
-- infrastructure, including:
--
-- * modeling user interface widgets
--
-- * managing changes in focus between widgets
--
-- * managing widget state
--
-- This module does not provide any concrete widget types.  For
-- in-depth discussion on this module's API and widget implementation
-- in particular, see the Vty-ui User's Manual.
module Graphics.Vty.Widgets.Core
    (
    -- ** Widget Infrastructure
      WidgetImpl(..)
    , Widget
    , getNormalAttr
    , defaultContext
    , updateWidget
    , updateWidgetState
    , newWidget
    , getState
    , getCurrentSize
    , setCurrentPosition
    , getCurrentPosition
    , growVertical
    , growHorizontal
    , getCursorPosition
    , showWidget
    , getVisible
    , setVisible
    , (<~)
    , (<~~)

    -- ** Rendering
    , RenderContext(..)
    , RenderError(..)
    , render
    , renderAndPosition

    -- ** Miscellaneous
    , HasNormalAttr(..)
    , HasFocusAttr(..)
    , withNormalAttribute
    , withFocusAttribute

    -- ** Events
    , handleKeyEvent
    , onKeyPressed
    , onGainFocus
    , onLoseFocus
    , onResize
    , relayKeyEvents
    , relayFocusEvents

    -- ** Focus management
    , FocusGroup
    , FocusGroupError(..)
    , newFocusGroup
    , mergeFocusGroups
    , appendFocusGroup
    , resetFocusGroup
    , addToFocusGroup
    , focusNext
    , focusPrevious
    , setFocusGroupNextKey
    , setFocusGroupPrevKey
    , focus
    , unfocus
    )
where

import Data.Typeable
import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Exception
import Graphics.Vty
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Skins
import Graphics.Vty.Widgets.Events

-- |The class of types with a ''normal'' attribute.
class HasNormalAttr w where
    setNormalAttribute :: w -> Attr -> IO ()

-- |The class of types with a ''focus'' attribute, i.e., a way of
-- visually indicating that the object has input focus.
class HasFocusAttr w where
    setFocusAttribute :: w -> Attr -> IO ()

instance HasNormalAttr (Widget a) where
    setNormalAttribute wRef a =
        updateWidget wRef $ \w -> w { normalAttribute = mergeAttr a (normalAttribute w) }

instance HasFocusAttr (Widget a) where
    setFocusAttribute wRef a =
        updateWidget wRef $ \w -> w { focusAttribute = mergeAttr a (focusAttribute w) }

-- |Set the normal attribute on a value.
withNormalAttribute :: (HasNormalAttr w) => Attr -> w -> IO w
withNormalAttribute att w = do
  setNormalAttribute w att
  return w

-- |Set the focus attribute on a value.
withFocusAttribute :: (HasFocusAttr w) => Attr -> w -> IO w
withFocusAttribute att w = do
  setFocusAttribute w att
  return w

-- |Render errors.
data RenderError = ImageTooBig String DisplayRegion DisplayRegion
                   -- ^An error indicating that a widget rendered to
                   -- an image which exceeded the available space.
                   -- Provides a representation of the violating
                   -- widget, the size of the available space, and the
                   -- size of the image which the widget's rendering
                   -- routine produced.
                   deriving (Show, Typeable)

instance Exception RenderError

-- |Context information used during the rendering process.
data RenderContext =
    RenderContext { normalAttr :: Attr
                  -- ^The default normal attribute to use unless
                  -- overridden by a given widget.
                  , focusAttr :: Attr
                  -- ^The default focused attribute to use for a
                  -- focused widget unless overridden by a given
                  -- widget.
                  , overrideAttr :: Attr
                  -- ^An override attribute to be used to override
                  -- both the normal and focus attributes in effect
                  -- during rendering.  Usually def_attr, this
                  -- attribute is used when child widgets need to have
                  -- their attributes overridden by a parent widget.
                  , skin :: Skin
                  -- ^The skin to use for rendering borders and other
                  -- interface elements which use the skin for their
                  -- representations.
                  }

-- |Get the normal attribute of a rendering context.
getNormalAttr :: RenderContext -> Attr
getNormalAttr ctx = mergeAttrs [ overrideAttr ctx, normalAttr ctx ]

-- |Default context settings.
defaultContext :: RenderContext
defaultContext = RenderContext def_attr (white `on` blue) def_attr unicodeSkin

-- |The type of widget implementations, parameterized on the type of
-- the widget's state.
data WidgetImpl a = WidgetImpl {
      state :: !a
    -- ^The state of the widget.
    , visible :: !Bool
    -- ^Whether the widget is visible.
    , render_ :: Widget a -> DisplayRegion -> RenderContext -> IO Image
    -- ^The rendering routine of the widget.  Takes the widget itself,
    -- a region indicating how much space the rendering process has to
    -- work with, and a rendering context to be used to determine
    -- attribute and skin settings.  This MUST return an image which
    -- is no larger than the specified rendering region.
    , growHorizontal_ :: a -> IO Bool
    -- ^Returns whether the widget will automatically grow to fill
    -- available horizontal space.
    , growVertical_ :: a -> IO Bool
    -- ^Returns whether the widget will automatically grow to fill
    -- available vertical space.
    , currentSize :: DisplayRegion
    -- ^The size of the widget after its most recent rendering pass.
    , currentPosition :: DisplayRegion
    -- ^The position of the widget after its most recent rendering
    -- pass.
    , normalAttribute :: Attr
    -- ^The normal (unfocused) attribute of the wiget.
    , focusAttribute :: Attr
    -- ^The focused attribute of the widget.
    , setCurrentPosition_ :: Widget a -> DisplayRegion -> IO ()
    -- ^Sets the current position of the widget.  Takes a widget
    -- reference and a display region indicating the coordinates of
    -- the widget's upper left corner.
    , keyEventHandler :: Widget a -> Key -> [Modifier] -> IO Bool
    -- ^The widget's key event handler.  Takes a widget reference, a
    -- key event, and a list of keyboard modifiers.  Returns whether
    -- the keyboard event was handled.  True indicates that the event
    -- was handled and that event processing should halt; False
    -- indicates that other event handlers, if present, may handle the
    -- event.
    , gainFocusHandlers :: Handlers (Widget a)
    -- ^List of handlers to be invoked when the widget gains focus.
    , resizeHandlers :: Handlers (DisplayRegion, DisplayRegion)
    -- ^List of handlers to be invoked when the widget's size changes.
    , loseFocusHandlers :: Handlers (Widget a)
    -- ^List of handlers to be invoked when the widget loses focus.
    , focused :: Bool
    -- ^Whether the widget is focused.
    , getCursorPosition_ :: Widget a -> IO (Maybe DisplayRegion)
    -- ^Returns the current terminal cursor position.  Should return
    -- Nothing if the widget does not need to show a cursor, or Just
    -- if it does.  (For example, widgets receiving keyboard input for
    -- text editing would should a cursor, but most won't need to.)
    }

type Widget a = IORef (WidgetImpl a)

-- |Show a widget.  Most widget show instances aren't going to contain
-- all of the widget state, but this at least gives an indication of
-- the widget type, which can be crucial for debugging.
showWidget :: (Show a) => Widget a -> IO String
showWidget wRef = show <$> readIORef wRef

instance (Show a) => Show (WidgetImpl a) where
    show w = concat [ "WidgetImpl { "
                    , show $ state w
                    , ", currentSize = "
                    , show $ currentSize w
                    , ", currentPosition = "
                    , show $ currentPosition w
                    , ", focused = "
                    , show $ focused w
                    , " }"
                    ]

-- |Set the visibility of a widget.  Invisible widgets do not grow in
-- either direction, always render to an empty image, and never
-- declare a cursor position.
setVisible :: Widget a -> Bool -> IO ()
setVisible wRef v = updateWidget wRef $ \st -> st { visible = v }

-- |Get the visibility of a widget.
getVisible :: Widget a -> IO Bool
getVisible = (visible <~)

-- |Does a widget grow horizontally?
growHorizontal :: Widget a -> IO Bool
growHorizontal w = do
  v <- visible <~ w
  case v of
    True -> do
           act <- growHorizontal_ <~ w
           st <- state <~ w
           act st
    False -> return False

-- |Does a widget grow vertically?
growVertical :: Widget a -> IO Bool
growVertical w = do
  v <- visible <~ w
  case v of
    True -> do
           act <- growVertical_ <~ w
           st <- state <~ w
           act st
    False -> return False

-- |Render a widget.  This function should be called by widget
-- implementations, since it does more than 'render_'; this function
-- takes care of setting up attributes in the rendering context,
-- setting the size of the widget after it has been rendered, and
-- checking for size violations.  May throw a 'RenderError'.
render :: (Show a) =>
          Widget a -- ^The widget to render.
       -> DisplayRegion -- ^The amount of space in which to render the
                        -- widget.
       -> RenderContext -- ^The rendering context to use.
       -> IO Image
render wRef sz ctx = do
  impl <- readIORef wRef

  v <- visible <~ wRef
  case v of
    False -> return empty_image
    True -> do
           -- Merge the override attributes with the context.  If the
           -- overrides haven't been set (still def_attr), they will
           -- have no effect on the context attributes.
           norm <- normalAttribute <~ wRef
           foc <- focusAttribute <~ wRef
           let newCtx = ctx { normalAttr = mergeAttr norm $ normalAttr ctx
                            , focusAttr = mergeAttr foc $ focusAttr ctx
                            }

           img <- render_ impl wRef sz newCtx
           let imgsz =  DisplayRegion (image_width img) (image_height img)

           when (image_width img > region_width sz ||
                 image_height img > region_height sz) $ throw $ ImageTooBig (show impl) sz imgsz

           setCurrentSize wRef $ DisplayRegion (image_width img) (image_height img)
           return img

-- |Render a widget and set its position after rendering is complete.
-- This is exported for internal use; widget implementations should
-- call 'render' instead.
renderAndPosition :: (Show a) =>
                     Widget a -- ^The widget to render.
                  -> DisplayRegion -- ^The position at which to render
                                   -- the widget.
                  -> DisplayRegion -- ^The amount of space in which to
                                   -- render the widget.
                  -> RenderContext -- ^The rendering context to use.
                  -> IO Image
renderAndPosition wRef pos sz ctx = do
  img <- render wRef sz ctx
  -- Position post-processing depends on the sizes being correct!
  setCurrentPosition wRef pos
  return img

-- |Set the current size of a widget.  Exported for internal use.  When the
-- size changes from its previous value, resize event handlers will be invoked.
setCurrentSize :: Widget a -> DisplayRegion -> IO ()
setCurrentSize wRef newSize = do
    oldSize <- getCurrentSize wRef
    modifyIORef wRef $ \w ->
        let new =  w { currentSize = newSize }
        in seq new new
    when (oldSize /= newSize) $ handleResizeEvent wRef (oldSize, newSize)

-- |Get the current size of the widget (its size after its most recent
-- rendering).
getCurrentSize :: Widget a -> IO DisplayRegion
getCurrentSize wRef = (return . currentSize) =<< (readIORef wRef)

-- |Set the current position of a widget.
getCurrentPosition :: Widget a -> IO DisplayRegion
getCurrentPosition wRef = currentPosition <$> (readIORef wRef)

-- |Set the current position of a widget.  Exported for internal use.
setCurrentPosition :: Widget a -> DisplayRegion -> IO ()
setCurrentPosition wRef pos = do
  updateWidget wRef $ \w -> w { currentPosition = pos }
  w <- readIORef wRef
  (setCurrentPosition_ w) wRef pos

-- |Create a new widget.  Takes an initial state value and a widget
-- implementation transformation and passes it an implementation with
-- default values.
newWidget :: a
          -> (WidgetImpl a -> WidgetImpl a)
          -> IO (Widget a)
newWidget initState f = do
  gfhs <- newHandlers
  lfhs <- newHandlers
  rhs <- newHandlers

  wRef <- newIORef $
          WidgetImpl { state = initState
                     , render_ = \_ _ _ -> return empty_image
                     , growVertical_ = const $ return False
                     , growHorizontal_ = const $ return False
                     , setCurrentPosition_ = \_ _ -> return ()
                     , currentSize = DisplayRegion 0 0
                     , currentPosition = DisplayRegion 0 0
                     , focused = False
                     , visible = True
                     , gainFocusHandlers = gfhs
                     , resizeHandlers = rhs
                     , loseFocusHandlers = lfhs
                     , keyEventHandler = \_ _ _ -> return False
                     , getCursorPosition_ = defaultCursorInfo
                     , normalAttribute = def_attr
                     , focusAttribute = def_attr
                     }

  updateWidget wRef f
  return wRef

-- |Default cursor positioning implementation used by 'newWidget'.
defaultCursorInfo :: Widget a -> IO (Maybe DisplayRegion)
defaultCursorInfo w = do
  sz <- getCurrentSize w
  pos <- getCurrentPosition w
  if region_width sz > 0 then
      return $ Just $ pos `plusWidth` (region_width sz - 1) else
      return Nothing

-- |Given a widget and key event information, invoke the widget's key
-- event handler with the event information.  Returns whether the
-- event was handled.
handleKeyEvent :: Widget a -> Key -> [Modifier] -> IO Bool
handleKeyEvent wRef keyEvent mods = do
  act <- keyEventHandler <~ wRef
  act wRef keyEvent mods

-- |Given a widget, invoke its resize event handlers with the old and new
-- sizes.
handleResizeEvent :: Widget a -> (DisplayRegion, DisplayRegion) -> IO ()
handleResizeEvent wRef szs = fireEvent wRef (resizeHandlers <~) szs

-- |Given widgets A and B, causes any key events on widget A to be
-- relayed to widget B.  Note that this does behavior constitutes an
-- ordinary key event handler from A's perspective, so if B does not
-- handle a given key event, subsequent key event handlers on A will
-- still get a chance to handle the event.  This function is mostly
-- useful for wrapper widgets which don't do any event handling of
-- their own but want to ensure that all key events are relayed to the
-- wrapped widget.
relayKeyEvents :: Widget a -> Widget b -> IO ()
relayKeyEvents a b = a `onKeyPressed` \_ k mods -> handleKeyEvent b k mods

-- |Given widgets A and B, cause all focus gain and loss events on A
-- to cause focus gain and loss for B.
relayFocusEvents :: Widget a -> Widget b -> IO ()
relayFocusEvents a b = do
  a `onGainFocus` \_ -> focus b
  a `onLoseFocus` \_ -> unfocus b

-- |Given a widget and a key event handler, add the handler to the
-- widget's key event handler structure.  The event handler is added
-- last, so any preexisting handlers will run before this one.
onKeyPressed :: Widget a -> (Widget a -> Key -> [Modifier] -> IO Bool) -> IO ()
onKeyPressed wRef handler = do
  -- Create a new handler that calls this one but defers to the old
  -- one if the new one doesn't handle the event.
  oldHandler <- keyEventHandler <~ wRef

  let combinedHandler =
          \w k ms -> do
            v <- handler w k ms
            case v of
              True -> return True
              False -> oldHandler w k ms

  updateWidget wRef $ \w -> w { keyEventHandler = combinedHandler }

-- |Focus a widget.  Causes its focus gain event handlers to run.  If
-- the widget is in a 'FocusGroup' and if that group's
-- currently-focused widget is some other widget, that widget will
-- lose the focus and its focus loss event handlers will be called.
focus :: Widget a -> IO ()
focus wRef = do
  updateWidget wRef $ \w -> w { focused = True }
  fireEvent wRef (gainFocusHandlers <~) wRef

-- |Unfocus a widget.  Causes its focus loss event handlers to run.
unfocus :: Widget a -> IO ()
unfocus wRef = do
  updateWidget wRef $ \w -> w { focused = False }
  fireEvent wRef (loseFocusHandlers <~) wRef

-- |Given a widget and a focus gain event handler, add the handler to
-- the widget.  The handler will be invoked when the widget receives
-- focus.
onGainFocus :: Widget a -> (Widget a -> IO ()) -> IO ()
onGainFocus = addHandler (gainFocusHandlers <~)

-- |Given a widget and a resize event handler, add the handler to the widget.
-- The handler will be invoked when the widget's size changes.  This includes
-- the first rendering, at which point its size changes from (0, 0).  Note that
-- if the resize handler needs to change the visual appearance of the widget
-- when its size changes, be sure to use 'schedule' to ensure that visual
-- changes are reflected immediately, and be absolutely sure that those changes
-- will not cause further size changes; that will cause a resize event handler
-- loop that will consume your CPU!
onResize :: Widget a -> ((DisplayRegion, DisplayRegion) -> IO ()) -> IO ()
onResize = addHandler (resizeHandlers <~)

-- |Given a widget and a focus loss event handler, add the handler to
-- the widget.  The handler will be invoked when the widget loses
-- focus.
onLoseFocus :: Widget a -> (Widget a -> IO ()) -> IO ()
onLoseFocus = addHandler (loseFocusHandlers <~)

-- |Convenience projection on the contents of an 'IORef'.
(<~) :: (a -> b) -> IORef a -> IO b
(<~) f wRef = (return . f) =<< (readIORef wRef)

-- |Convenience projection on the state of a widget.
(<~~) :: (a -> b) -> Widget a -> IO b
(<~~) f wRef = (return . f . state) =<< (readIORef wRef)

-- |Given a widget and an implementation transformer, apply the
-- transformer to the widget's implementation.
updateWidget :: Widget a -> (WidgetImpl a -> WidgetImpl a) -> IO ()
updateWidget wRef f = modifyIORef wRef $ \val -> let new = f val
                                                 in seq new new

-- |Get the state value of a widget.
getState :: Widget a -> IO a
getState wRef = state <~ wRef

-- |Apply a state transformation function to a widget's state.
updateWidgetState :: Widget a -> (a -> a) -> IO ()
updateWidgetState wRef f = do
  w <- readIORef wRef
  writeIORef wRef $ let new = w { state = f (state w) }
                    in seq new new

-- |Focus group handling errors.
data FocusGroupError = FocusGroupEmpty
                     -- ^Thrown when the desired operation could not
                     -- be completed because the focus group is empty.
                     | FocusGroupBadIndex Int
                       -- ^Thrown when the specified focus group entry
                       -- index was invalid.
                       deriving (Typeable, Show)

instance Exception FocusGroupError

-- |The state type of widgets added to a focus group.
data FocusEntry = forall a. FocusEntry (Widget a)

-- |Focus group.  Represents an cycle of widgets which receive input
-- focus.
data FocusGroup = FocusGroup { entries :: [Widget FocusEntry]
                             , currentEntryNum :: Int
                             , nextKey :: (Key, [Modifier])
                             , prevKey :: (Key, [Modifier])
                             }

newFocusEntry :: (Show a) => Widget a -> IO (Widget FocusEntry)
newFocusEntry chRef = do

  let st = FocusEntry chRef

  wRef <- newWidget st $ \w ->
      w { growHorizontal_ = const $ growHorizontal chRef
        , growVertical_ = const $ growVertical chRef

        , render_ = \_ sz ctx -> render chRef sz ctx

        , setCurrentPosition_ =
            \this pos -> do
              (FocusEntry ch) <- getState this
              setCurrentPosition ch pos
        }

  wRef `relayFocusEvents` chRef
  wRef `relayKeyEvents` chRef

  return wRef

-- |Create a new focus group.  Note that the focus group is itself a
-- widget; any input event handlers added to the focus group will fire
-- before input events are handled by the currently-focused widget.
newFocusGroup :: IO (Widget FocusGroup)
newFocusGroup = do

  let initSt = FocusGroup { entries = []
                          , currentEntryNum = -1
                          , nextKey = (KASCII '\t', [])
                          , prevKey = (KBackTab, [])
                          }

  wRef <- newWidget initSt $ \w ->
      w { getCursorPosition_ =
            \this -> do
              cur <- currentEntryNum <~~ this
              case cur of
                (-1) -> return Nothing
                _ -> do
                  eRef <- currentEntry this
                  (FocusEntry e) <- state <~ eRef
                  getCursorPosition e

        , keyEventHandler =
            \this key mods -> do
              st <- getState this
              case currentEntryNum st of
                (-1) -> return False
                i -> do
                  if (key, mods) == nextKey st then
                      (focusNext this >> return True) else
                      if (key, mods) == prevKey st then
                            (focusPrevious this >> return True) else
                          do
                            let e = entries st !! i
                            handleKeyEvent e key mods
        }
  return wRef

-- |Set the keyboard event information used to change focus to the
-- next widget in a focus group.
setFocusGroupNextKey :: Widget FocusGroup -> Key -> [Modifier] -> IO ()
setFocusGroupNextKey fg k mods =
    updateWidgetState fg $ \s -> s { nextKey = (k, mods) }

-- |Set the keyboard event information used to change focus to the
-- previous widget in a focus group.
setFocusGroupPrevKey :: Widget FocusGroup -> Key -> [Modifier] -> IO ()
setFocusGroupPrevKey fg k mods =
    updateWidgetState fg $ \s -> s { prevKey = (k, mods) }

-- |Merge two focus groups.  Given two focus groups A and B, this
-- returns a new focus group with all of the entries from A and B
-- added to it, in that order.  Both A and B must be non-empty or
-- 'FocusGroupEmpty' will be thrown.
mergeFocusGroups :: Widget FocusGroup -> Widget FocusGroup -> IO (Widget FocusGroup)
mergeFocusGroups a b = do
  c <- newFocusGroup

  aEntries <- entries <~~ a
  bEntries <- entries <~~ b

  when (null aEntries || null bEntries) $
       throw FocusGroupEmpty

  updateWidgetState c $ \s -> s { entries = aEntries ++ bEntries
                                , currentEntryNum = 0
                                }

  -- Now we need to be sure that we have the event handlers set
  -- correctly on each widget.  The reason we don't just call
  -- addToFocusGroup on each entry's widget is because the user may
  -- have added event handlers to the FocusEntries themselves, and we
  -- want to preserve those, so we extract the widget from the focus
  -- entry to add the onGainFocus handler, but use the existing
  -- FocusEntries when constructing the new focus group.
  forM_ (zip [0..] aEntries) $ \(i, e) -> do
    (FocusEntry w) <- state <~ e
    w `onGainFocus` (const $ setCurrentFocus c i)

  forM_ (zip [(length aEntries)..] bEntries) $ \(i, e) -> do
    (FocusEntry w) <- state <~ e
    w `onGainFocus` (const $ setCurrentFocus c i)

  return c

-- |Given two focus groups A and B, append the entries of B to A,
-- mutating A in the process.  Throws 'FocusGroupEmpty' if B is empty.
appendFocusGroup :: Widget FocusGroup -> Widget FocusGroup -> IO ()
appendFocusGroup a b = do
  aEntries <- entries <~~ a
  bEntries <- entries <~~ b

  when (null bEntries) $
       throw FocusGroupEmpty

  updateWidgetState a $ \s -> s { entries = (entries s) ++ bEntries
                                , currentEntryNum = 0
                                }

  -- Now we need to be sure that we have the event handlers set
  -- correctly on each widget.  The reason we don't just call
  -- addToFocusGroup on each entry's widget is because the user may
  -- have added event handlers to the FocusEntries themselves, and we
  -- want to preserve those, so we extract the widget from the focus
  -- entry to add the onGainFocus handler, but use the existing
  -- FocusEntries when constructing the new focus group.
  forM_ (zip [(length aEntries)..] bEntries) $ \(i, e) -> do
    (FocusEntry w) <- state <~ e
    w `onGainFocus` (const $ setCurrentFocus a i)

-- |Reset a focus group.  This ensures that the focus group's state is
-- coherent by calling 'focus' on the group's focused entry and
-- 'unfocus' on all the rest.  This is for internal use, but is used
-- by the 'Collection' switching implementation to ensure that focus
-- state is sane.
resetFocusGroup :: Widget FocusGroup -> IO ()
resetFocusGroup fg = do
  cur <- currentEntryNum <~~ fg
  es <- entries <~~ fg
  forM_ (zip [0..] es) $ \(i, e) ->
      when (i /= cur) $ unfocus e
  when (cur >= 0) $
       focus =<< currentEntry fg

-- |Get the desired cursor position, if any, for a widget.
getCursorPosition :: Widget a -> IO (Maybe DisplayRegion)
getCursorPosition wRef = do
  v <- visible <~ wRef
  case v of
    True -> do
           ci <- getCursorPosition_ <~ wRef
           ci wRef
    False -> return Nothing

-- |Return the current focus entry of a focus group.
currentEntry :: Widget FocusGroup -> IO (Widget FocusEntry)
currentEntry wRef = do
  es <- entries <~~ wRef
  i <- currentEntryNum <~~ wRef
  when (i == -1) $ throw FocusGroupEmpty
  return (es !! i)

-- |Add a widget to a focus group.  This returns a focus group entry
-- which wraps the specified widget; the focus group entry is also a
-- widget and can take key event handlers and the like.  During input
-- event processing, the focus group entry receives keyboard events
-- and passes them on to the wrapped widget.  If you want a widget to
-- have specific event handling in a particular interface, add event
-- handlers to its focus entry/entries instead of the widget itself.
addToFocusGroup :: (Show a) => Widget FocusGroup -> Widget a -> IO (Widget FocusEntry)
addToFocusGroup cRef wRef = do
  eRef <- newFocusEntry wRef

  entryPos <- (length . entries) <~~ cRef
  updateWidgetState cRef $ \s -> s { entries = (entries s) ++ [eRef] }

  -- Add an event handler to the widget, NOT the entry wrapper, so
  -- others can call 'focus' on the widget and affect this focus
  -- group.
  wRef `onGainFocus` \_ ->
      setCurrentFocus cRef entryPos

  -- If we just added the first widget to the group, focus it so
  -- something has focus.
  when (entryPos == 0) $ focus eRef

  return eRef

-- |Focus the next widget in a focus group.
focusNext :: Widget FocusGroup -> IO ()
focusNext wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  let nextEntry = if cur < length (entries st) - 1 then
                      (entries st) !! (cur + 1) else
                      (entries st) !! 0
  focus nextEntry

-- |Focus the previous widget in a focus group.
focusPrevious :: Widget FocusGroup -> IO ()
focusPrevious wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  let prevEntry = if cur > 0 then
                      (entries st) !! (cur - 1) else
                      (entries st) !! (length (entries st) - 1)
  focus prevEntry

-- Note that this only 1) updates the focus index in the group and 2)
-- calls unfocus on the previously-focused widget.  This does NOT call
-- focus on the newly-focused widget, because this is intended to be
-- callable from a focus event handler for the widget that got
-- focused.
setCurrentFocus :: Widget FocusGroup -> Int -> IO ()
setCurrentFocus cRef i = do
  st <- state <~ cRef

  when (i >= length (entries st) || i < 0) $
       throw $ FocusGroupBadIndex i

  -- If new entry number is different from existing one, invoke focus
  -- handlers.
  when (currentEntryNum st /= i) $
       do
         when (currentEntryNum st >= 0) $
              unfocus ((entries st) !! (currentEntryNum st))

  updateWidgetState cRef $ \s -> s { currentEntryNum = i }
