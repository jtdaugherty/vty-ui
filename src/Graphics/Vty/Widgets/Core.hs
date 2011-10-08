{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances #-}
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

class HasNormalAttr w where
    setNormalAttribute :: w -> Attr -> IO ()

class HasFocusAttr w where
    setFocusAttribute :: w -> Attr -> IO ()

instance HasNormalAttr (Widget a) where
    setNormalAttribute wRef a =
        updateWidget wRef $ \w -> w { normalAttribute = mergeAttr a (normalAttribute w) }

instance HasFocusAttr (Widget a) where
    setFocusAttribute wRef a =
        updateWidget wRef $ \w -> w { focusAttribute = mergeAttr a (focusAttribute w) }

withNormalAttribute :: (HasNormalAttr w) => Attr -> w -> IO w
withNormalAttribute att w = do
  setNormalAttribute w att
  return w

withFocusAttribute :: (HasFocusAttr w) => Attr -> w -> IO w
withFocusAttribute att w = do
  setFocusAttribute w att
  return w

data RenderError = ImageTooBig String DisplayRegion DisplayRegion
                   deriving (Show, Typeable)

instance Exception RenderError

data RenderContext = RenderContext { normalAttr :: Attr
                                   , focusAttr :: Attr
                                   , overrideAttr :: Attr
                                   , skin :: Skin
                                   }

getNormalAttr :: RenderContext -> Attr
getNormalAttr ctx = mergeAttrs [ overrideAttr ctx, normalAttr ctx ]

defaultContext :: RenderContext
defaultContext = RenderContext def_attr def_attr def_attr unicodeSkin

data WidgetImpl a = WidgetImpl {
      state :: a
    , render_ :: Widget a -> DisplayRegion -> RenderContext -> IO Image
    , growHorizontal_ :: a -> IO Bool
    , growVertical_ :: a -> IO Bool
    , currentSize :: DisplayRegion
    , currentPosition :: DisplayRegion
    , normalAttribute :: Attr
    , focusAttribute :: Attr
    , setCurrentPosition_ :: Widget a -> DisplayRegion -> IO ()
    , keyEventHandler :: Widget a -> Key -> [Modifier] -> IO Bool
    , gainFocusHandlers :: Handlers (Widget a)
    , loseFocusHandlers :: Handlers (Widget a)
    , focused :: Bool
    , getCursorPosition_ :: Widget a -> IO (Maybe DisplayRegion)
    }

type Widget a = IORef (WidgetImpl a)

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

growHorizontal :: Widget a -> IO Bool
growHorizontal w = do
  act <- growHorizontal_ <~ w
  st <- state <~ w
  act st

growVertical :: Widget a -> IO Bool
growVertical w = do
  act <- growVertical_ <~ w
  st <- state <~ w
  act st

render :: (Show a) => Widget a -> DisplayRegion -> RenderContext -> IO Image
render wRef sz ctx = do
  impl <- readIORef wRef

  -- Merge the override attributes with the context.  If the overrides
  -- haven't been set (still def_attr), they will have no effect on
  -- the context attributes.
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

renderAndPosition :: (Show a) => Widget a -> DisplayRegion -> DisplayRegion
                  -> RenderContext -> IO Image
renderAndPosition wRef pos sz ctx = do
  img <- render wRef sz ctx
  -- Position post-processing depends on the sizes being correct!
  setCurrentPosition wRef pos
  return img

setCurrentSize :: Widget a -> DisplayRegion -> IO ()
setCurrentSize wRef newSize =
    modifyIORef wRef $ \w -> w { currentSize = newSize }

getCurrentSize :: Widget a -> IO DisplayRegion
getCurrentSize wRef = (return . currentSize) =<< (readIORef wRef)

getCurrentPosition :: Widget a -> IO DisplayRegion
getCurrentPosition wRef = currentPosition <$> (readIORef wRef)

setCurrentPosition :: Widget a -> DisplayRegion -> IO ()
setCurrentPosition wRef pos = do
  updateWidget wRef $ \w -> w { currentPosition = pos }
  w <- readIORef wRef
  (setCurrentPosition_ w) wRef pos

newWidget :: (WidgetImpl a -> WidgetImpl a) -> IO (Widget a)
newWidget f = do
  gfhs <- newHandlers
  lfhs <- newHandlers

  wRef <- newIORef $
          WidgetImpl { state = undefined
                     , render_ = undefined
                     , growVertical_ = const $ return False
                     , growHorizontal_ = const $ return False
                     , setCurrentPosition_ = \_ _ -> return ()
                     , currentSize = DisplayRegion 0 0
                     , currentPosition = DisplayRegion 0 0
                     , focused = False
                     , gainFocusHandlers = gfhs
                     , loseFocusHandlers = lfhs
                     , keyEventHandler = \_ _ _ -> return False
                     , getCursorPosition_ = defaultCursorInfo
                     , normalAttribute = def_attr
                     , focusAttribute = def_attr
                     }

  updateWidget wRef f
  return wRef

defaultCursorInfo :: Widget a -> IO (Maybe DisplayRegion)
defaultCursorInfo w = do
  sz <- getCurrentSize w
  pos <- getCurrentPosition w
  return $ Just $ pos `plusWidth` (region_width sz - 1)

handleKeyEvent :: Widget a -> Key -> [Modifier] -> IO Bool
handleKeyEvent wRef keyEvent mods = do
  act <- keyEventHandler <~ wRef
  act wRef keyEvent mods

relayKeyEvents :: Widget a -> Widget b -> IO ()
relayKeyEvents a b = a `onKeyPressed` \_ k mods -> handleKeyEvent b k mods

relayFocusEvents :: Widget a -> Widget b -> IO ()
relayFocusEvents a b = do
  a `onGainFocus` \_ -> focus b
  a `onLoseFocus` \_ -> unfocus b

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

focus :: Widget a -> IO ()
focus wRef = do
  updateWidget wRef $ \w -> w { focused = True }
  fireEvent wRef (gainFocusHandlers <~) wRef

unfocus :: Widget a -> IO ()
unfocus wRef = do
  updateWidget wRef $ \w -> w { focused = False }
  fireEvent wRef (loseFocusHandlers <~) wRef

onGainFocus :: Widget a -> (Widget a -> IO ()) -> IO ()
onGainFocus = addHandler (gainFocusHandlers <~)

onLoseFocus :: Widget a -> (Widget a -> IO ()) -> IO ()
onLoseFocus = addHandler (loseFocusHandlers <~)

(<~) :: (a -> b) -> IORef a -> IO b
(<~) f wRef = (return . f) =<< (readIORef wRef)

(<~~) :: (a -> b) -> Widget a -> IO b
(<~~) f wRef = (return . f . state) =<< (readIORef wRef)

updateWidget :: Widget a -> (WidgetImpl a -> WidgetImpl a) -> IO ()
updateWidget wRef f = modifyIORef wRef f

getState :: Widget a -> IO a
getState wRef = state <~ wRef

updateWidgetState :: Widget a -> (a -> a) -> IO ()
updateWidgetState wRef f = do
  w <- readIORef wRef
  writeIORef wRef $ w { state = f (state w) }

data FocusGroupError = FocusGroupEmpty
                     | FocusGroupBadIndex Int
                       deriving (Typeable, Show)

instance Exception FocusGroupError

data FocusEntry = forall a. FocusEntry (Widget a)

data FocusGroup = FocusGroup { entries :: [Widget FocusEntry]
                             , currentEntryNum :: Int
                             , nextKey :: (Key, [Modifier])
                             , prevKey :: (Key, [Modifier])
                             }

newFocusEntry :: (Show a) => Widget a -> IO (Widget FocusEntry)
newFocusEntry chRef = do
  wRef <- newWidget $ \w ->
      w { state = FocusEntry chRef

        , growHorizontal_ = const $ growHorizontal chRef
        , growVertical_ = const $ growVertical chRef

        , render_ =
            \_ sz ctx -> render chRef sz ctx

        , setCurrentPosition_ =
            \this pos -> do
              (FocusEntry ch) <- getState this
              setCurrentPosition ch pos
        }

  wRef `relayFocusEvents` chRef
  wRef `relayKeyEvents` chRef

  return wRef

newFocusGroup :: IO (Widget FocusGroup)
newFocusGroup = do
  wRef <- newWidget $ \w ->
      w { state = FocusGroup { entries = []
                             , currentEntryNum = -1
                             , nextKey = (KASCII '\t', [])
                             , prevKey = (KASCII '\t', [MShift])
                             }

        , getCursorPosition_ =
            \this -> do
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

        -- Should never be rendered.
        , render_ = \_ _ _ -> return empty_image
        }
  return wRef

setFocusGroupNextKey :: Widget FocusGroup -> Key -> [Modifier] -> IO ()
setFocusGroupNextKey fg k mods =
    updateWidgetState fg $ \s -> s { nextKey = (k, mods) }

setFocusGroupPrevKey :: Widget FocusGroup -> Key -> [Modifier] -> IO ()
setFocusGroupPrevKey fg k mods =
    updateWidgetState fg $ \s -> s { prevKey = (k, mods) }

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

resetFocusGroup :: Widget FocusGroup -> IO ()
resetFocusGroup fg = do
  cur <- currentEntryNum <~~ fg
  es <- entries <~~ fg
  forM_ (zip [0..] es) $ \(i, e) ->
      when (i /= cur) $ unfocus e
  when (cur >= 0) $
       focus =<< currentEntry fg

getCursorPosition :: Widget a -> IO (Maybe DisplayRegion)
getCursorPosition wRef = do
  ci <- getCursorPosition_ <~ wRef
  ci wRef

currentEntry :: Widget FocusGroup -> IO (Widget FocusEntry)
currentEntry wRef = do
  es <- entries <~~ wRef
  i <- currentEntryNum <~~ wRef
  when (i == -1) $ throw FocusGroupEmpty
  return (es !! i)

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

focusNext :: Widget FocusGroup -> IO ()
focusNext wRef = do
  st <- getState wRef
  let cur = currentEntryNum st
  when (cur == -1) $ throw FocusGroupEmpty
  let nextEntry = if cur < length (entries st) - 1 then
                      (entries st) !! (cur + 1) else
                      (entries st) !! 0
  focus nextEntry

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
