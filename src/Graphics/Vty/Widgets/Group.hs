-- |Widget groups.  Useful for when you need to swap out many
-- instances of the same widget type in a specific location in an
-- interface.  A group has a currently active widget which can be
-- changed with 'setCurrentGroupEntry'.  Add a widget to a group with
-- 'addToGroup'; 'addToGroup' returns an action which can be used to
-- set the specified widget as the group's active widget.
module Graphics.Vty.Widgets.Group
    ( Group
    , newGroup
    , addToGroup
    , setCurrentGroupEntry
    )
where

import Control.Monad ( when )
import Graphics.Vty ( empty_image )
import Graphics.Vty.Widgets.Core

-- |A group of widgets of a specified type.
data Group a = Group { entries :: [Widget a]
                     , currentEntryNum :: Int
                     }

instance Show (Group a) where
    show grp = concat [ "Group {"
                      , " entries = (" ++ (show $ length $ entries grp) ++ ")"
                      , ", currentEntryNum = " ++ (show $ currentEntryNum grp)
                      , "}"
                      ]

-- |Create a new empty widget group.
newGroup :: (Show a) => IO (Widget (Group a))
newGroup = do
  let initSt = Group { entries = []
                     , currentEntryNum = -1
                     }
  wRef <- newWidget initSt $ \w ->
      w { getCursorPosition_ =
            \this ->
                getCursorPosition =<< currentEntry this

        , setCurrentPosition_ =
            \this pos -> do
                e <- currentEntry this
                setCurrentPosition e pos

        , growHorizontal_ =
            \this ->
                growHorizontal $ currentEntry' this

        , growVertical_ =
            \this ->
                growVertical $ currentEntry' this

        , keyEventHandler =
            \this key mods -> do
              cur <- currentEntryNum <~~ this
              case cur of
                (-1) -> return False
                _ -> do
                  e <- currentEntry this
                  handleKeyEvent e key mods

        -- Should never be rendered.
        , render_ = \this sz ctx -> do
                      cur <- currentEntryNum <~~ this
                      case cur of
                        (-1) -> return empty_image
                        _ -> do
                          e <- currentEntry this
                          render e sz ctx
        }

  let focusCurrent = focus =<< currentEntry wRef
      unfocusCurrent = unfocus =<< currentEntry wRef

  wRef `onGainFocus` const focusCurrent
  wRef `onLoseFocus` const unfocusCurrent

  return wRef

currentEntry :: Widget (Group a) -> IO (Widget a)
currentEntry grp = do
  st <- getState grp
  return $ currentEntry' st

currentEntry' :: Group a -> Widget a
currentEntry' st = do
  case currentEntryNum st of
    (-1) -> error "currentEntry should not be called on an empty group"
    i -> entries st !! i

-- |Add a widget to a group.  Returns an action which, when evaluated,
-- will update the group state so that its currently-active widget is
-- the one passed to this function.
addToGroup :: Widget (Group a) -> Widget a -> IO (IO ())
addToGroup grp w = do
  numEntries <- (length . entries) <~~ grp
  updateWidgetState grp $ \s -> s { entries = entries s ++ [w]
                                  , currentEntryNum = if currentEntryNum s == (-1)
                                                      then 0
                                                      else currentEntryNum s
                                  }
  return $ setCurrentGroupEntry grp numEntries

-- |Set a group's current entry to the specified index.  Use with
-- care.
setCurrentGroupEntry :: Widget (Group a) -> Int -> IO ()
setCurrentGroupEntry grp i = do
  num <- (length . entries) <~~ grp
  when (i < 0 || i >= num) $ error "setCurrentGroupEntry got bad index"

  foc <- focused <~ grp
  cur <- currentEntryNum <~~ grp

  when (cur >= 0) $ do
    w <- currentEntry grp
    when foc $ unfocus w

  updateWidgetState grp $ \s -> s { currentEntryNum = i }

  w' <- currentEntry grp
  when foc $ focus w'
