{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
    , getEditText
    , setEditText
    , setEditCursorPosition
    , getEditCursorPosition
    , onActivate
    , onChange
    , onCursorMove
    )
where

import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data Edit = Edit { currentText :: String
                 , cursorPosition :: Int
                 , displayStart :: Int
                 , displayWidth :: Int
                 , activateHandlers :: IORef [Handler (Widget Edit)]
                 , changeHandlers :: IORef [Handler String]
                 , cursorMoveHandlers :: IORef [Handler Int]
                 }

instance Show Edit where
    show e = concat [ "Edit { "
                    , "currentText = ", show $ currentText e
                    , ", cursorPosition = ", show $ cursorPosition e
                    , ", displayStart = ", show $ displayStart e
                    , ", displayWidth = ", show $ displayWidth e
                    , " }"
                    ]

editWidget :: (MonadIO m) => m (Widget Edit)
editWidget = do
  wRef <- newWidget

  ahs <- mkHandlers
  chs <- mkHandlers
  cmhs <- mkHandlers

  updateWidget wRef $ \w ->
      w { state = Edit { currentText = ""
                       , cursorPosition = 0
                       , displayStart = 0
                       , displayWidth = 0
                       , activateHandlers = ahs
                       , changeHandlers = chs
                       , cursorMoveHandlers = cmhs
                       }

        , growHorizontal_ = const $ return True
        , cursorInfo =
            \this -> do
              f <- focused <~ this
              pos <- getCurrentPosition this
              curPos <- cursorPosition <~~ this
              start <- displayStart <~~ this

              if f then
                  return (Just $ pos `plusWidth` (toEnum (curPos - start))) else
                  return Nothing

        , render_ =
            \this size ctx -> do
              setDisplayWidth this (fromEnum $ region_width size)
              st <- getState this

              let truncated = take (displayWidth st)
                              (drop (displayStart st) (currentText st))

                  nAttr = mergeAttrs [ overrideAttr ctx
                                     , normalAttr ctx
                                     ]

              isFocused <- focused <~ this
              let attr = if isFocused then focusAttr ctx else nAttr

              return $ string attr truncated
                         <|> char_fill attr ' ' (region_width size - (toEnum $ length truncated)) 1

        , keyEventHandler = editKeyEvent
        }
  setNormalAttribute wRef $ style underline
  setFocusAttribute wRef $ style underline
  return wRef

onActivate :: (MonadIO m) => Widget Edit -> (Widget Edit -> IO ()) -> m ()
onActivate = addHandler (activateHandlers <~~)

notifyActivateHandlers :: (MonadIO m) => Widget Edit -> m ()
notifyActivateHandlers wRef = fireEvent wRef (activateHandlers <~~) wRef

notifyChangeHandlers :: Widget Edit -> IO ()
notifyChangeHandlers wRef = do
  s <- getEditText wRef
  fireEvent wRef (changeHandlers <~~) s

notifyCursorMoveHandlers :: (MonadIO m) => Widget Edit -> m ()
notifyCursorMoveHandlers wRef = do
  pos <- getEditCursorPosition wRef
  fireEvent wRef (cursorMoveHandlers <~~) pos

onChange :: (MonadIO m) => Widget Edit -> (String -> IO ()) -> m ()
onChange = addHandler (changeHandlers <~~)

onCursorMove :: (MonadIO m) => Widget Edit -> (Int -> IO ()) -> m ()
onCursorMove = addHandler (cursorMoveHandlers <~~)

getEditText :: (MonadIO m) => Widget Edit -> m String
getEditText = (currentText <~~)

setEditText :: (MonadIO m) => Widget Edit -> String -> m ()
setEditText wRef str = do
  updateWidgetState wRef $ \s -> s { currentText = str }
  liftIO $ do
    gotoBeginning wRef
    notifyChangeHandlers wRef

setEditCursorPosition :: (MonadIO m) => Widget Edit -> Int -> m ()
setEditCursorPosition wRef pos = do
  oldPos <- getEditCursorPosition wRef
  str <- getEditText wRef

  let newPos = if pos > (length str)
               then length str
               else if pos < 0
                    then 0
                    else pos

  when (newPos /= oldPos) $
       do
         updateWidgetState wRef $ \s ->
             s { cursorPosition = newPos
               }
         liftIO $ notifyCursorMoveHandlers wRef

getEditCursorPosition :: (MonadIO m) => Widget Edit -> m Int
getEditCursorPosition = (cursorPosition <~~)

setDisplayWidth :: Widget Edit -> Int -> IO ()
setDisplayWidth this width =
    updateWidgetState this $ \s ->
        let newDispStart = if cursorPosition s - displayStart s >= width
                           then cursorPosition s - width + 1
                           else displayStart s
        in s { displayWidth = width
             , displayStart = newDispStart
             }

editKeyEvent :: Widget Edit -> Key -> [Modifier] -> IO Bool
editKeyEvent this k mods = do
  case (k, mods) of
    (KASCII 'a', [MCtrl]) -> gotoBeginning this >> return True
    (KASCII 'k', [MCtrl]) -> killToEOL this >> return True
    (KASCII 'e', [MCtrl]) -> gotoEnd this >> return True
    (KASCII 'd', [MCtrl]) -> delCurrentChar this >> return True
    (KLeft, []) -> moveCursorLeft this >> return True
    (KRight, []) -> moveCursorRight this >> return True
    (KBS, []) -> deletePreviousChar this >> return True
    (KDel, []) -> delCurrentChar this >> return True
    (KASCII ch, []) -> insertChar this ch >> return True
    (KHome, []) -> gotoBeginning this >> return True
    (KEnd, []) -> gotoEnd this >> return True
    (KEnter, []) -> notifyActivateHandlers this >> return True
    _ -> return False

killToEOL :: Widget Edit -> IO ()
killToEOL this = do
  -- Preserve some state since setEditText changes it.
  pos <- cursorPosition <~~ this
  st <- displayStart <~~ this
  str <- getEditText this

  setEditText this $ take pos str
  updateWidgetState this $ \s ->
      s { displayStart = st
        }

  notifyChangeHandlers this

deletePreviousChar :: Widget Edit -> IO ()
deletePreviousChar this = do
  pos <- cursorPosition <~~ this
  when (pos /= 0) $ do
    moveCursorLeft this
    delCurrentChar this

gotoBeginning :: Widget Edit -> IO ()
gotoBeginning wRef = do
  updateWidgetState wRef $ \s -> s { displayStart = 0
                                   }
  setEditCursorPosition wRef 0

gotoEnd :: Widget Edit -> IO ()
gotoEnd wRef = do
  updateWidgetState wRef $ \s ->
      s { displayStart = if (length $ currentText s) > displayWidth s
                         then (length $ currentText s) - displayWidth s
                         else 0
        }
  s <- getEditText wRef
  setEditCursorPosition wRef $ length s

moveCursorLeft :: Widget Edit -> IO ()
moveCursorLeft wRef = do
  st <- getState wRef

  case cursorPosition st of
    0 -> return ()
    p -> do
      let newDispStart = if p == displayStart st
                         then displayStart st - 1
                         else displayStart st
      updateWidgetState wRef $ \s ->
          s { cursorPosition = p - 1
            , displayStart = newDispStart
            }
      notifyCursorMoveHandlers wRef

moveCursorRight :: Widget Edit -> IO ()
moveCursorRight wRef = do
  st <- getState wRef

  when (cursorPosition st < (length $ currentText st)) $
       do
         let newDispStart = if cursorPosition st == displayStart st + displayWidth st - 1
                            then displayStart st + 1
                            else displayStart st
         updateWidgetState wRef $ \s ->
             s { cursorPosition = cursorPosition st + 1
               , displayStart = newDispStart
               }
         notifyCursorMoveHandlers wRef

insertChar :: Widget Edit -> Char -> IO ()
insertChar wRef ch = do
  updateWidgetState wRef $ \st ->
      let newContent = inject (cursorPosition st) ch (currentText st)
          newViewStart =
              if cursorPosition st == displayStart st + displayWidth st - 1
              then displayStart st + 1
              else displayStart st
      in st { currentText = newContent
            , displayStart = newViewStart
            }
  moveCursorRight wRef
  notifyChangeHandlers wRef

delCurrentChar :: Widget Edit -> IO ()
delCurrentChar wRef = do
  st <- getState wRef
  when (cursorPosition st < (length $ currentText st)) $
       do
         let newContent = remove (cursorPosition st) (currentText st)
         updateWidgetState wRef $ \s -> s { currentText = newContent }
         notifyChangeHandlers wRef

remove :: Int -> [a] -> [a]
remove pos as = (take pos as) ++ (drop (pos + 1) as)

inject :: Int -> a -> [a] -> [a]
inject pos a as = let (h, t) = splitAt pos as
                  in h ++ (a:t)
