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

data Group a = Group { entries :: [Widget a]
                     , currentEntryNum :: Int
                     }

instance Show (Group a) where
    show grp = concat [ "Group {"
                      , " entries = (" ++ (show $ length $ entries grp) ++ ")"
                      , ", currentEntryNum = " ++ (show $ currentEntryNum grp)
                      , "}"
                      ]

newGroup :: (Show a) => IO (Widget (Group a))
newGroup = do
  wRef <- newWidget $ \w ->
      w { state = Group { entries = []
                        , currentEntryNum = -1
                        }

        , getCursorPosition_ =
            \this ->
                getCursorPosition =<< currentEntry this

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

addToGroup :: Widget (Group a) -> Widget a -> IO (IO ())
addToGroup grp w = do
  numEntries <- (length . entries) <~~ grp
  updateWidgetState grp $ \s -> s { entries = entries s ++ [w]
                                  , currentEntryNum = if currentEntryNum s == (-1)
                                                      then 0
                                                      else currentEntryNum s
                                  }
  return $ setCurrentGroupEntry grp numEntries

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
