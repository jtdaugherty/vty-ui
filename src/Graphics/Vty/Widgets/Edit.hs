{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |This module provides a one-line editing interface.
module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
    , getEditText
    , setEditText
    , setEditCursorPosition
    , getEditCursorPosition
    , setEditMaxLength
    , onActivate
    , onChange
    , onCursorMove
    )
where

import Control.Monad
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data Edit = Edit { currentText :: String
                 , cursorPosition :: Int
                 , displayStart :: Int
                 , displayWidth :: Int
                 , activateHandlers :: Handlers (Widget Edit)
                 , changeHandlers :: Handlers String
                 , cursorMoveHandlers :: Handlers Int
                 , maxTextLength :: Maybe Int
                 }

instance Show Edit where
    show e = concat [ "Edit { "
                    , "currentText = ", show $ currentText e
                    , ", cursorPosition = ", show $ cursorPosition e
                    , ", displayStart = ", show $ displayStart e
                    , ", displayWidth = ", show $ displayWidth e
                    , " }"
                    ]

-- |Create a new editing widget.
editWidget :: IO (Widget Edit)
editWidget = do
  ahs <- newHandlers
  chs <- newHandlers
  cmhs <- newHandlers

  let initSt = Edit { currentText = ""
                    , cursorPosition = 0
                    , displayStart = 0
                    , displayWidth = 0
                    , activateHandlers = ahs
                    , changeHandlers = chs
                    , cursorMoveHandlers = cmhs
                    , maxTextLength = Nothing
                    }

  wRef <- newWidget initSt $ \w ->
      w { growHorizontal_ = const $ return True
        , getCursorPosition_ =
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

-- |Set the maximum length of the edit widget's content.
setEditMaxLength :: Widget Edit -> Int -> IO ()
setEditMaxLength wRef v = do
  cur <- maxTextLength <~~ wRef
  case cur of
    Nothing -> return ()
    Just oldMax ->
        when (v < oldMax) $
             do
               s <- currentText <~~ wRef
               setEditText wRef $ take v s
  updateWidgetState wRef $ \s -> s { maxTextLength = Just v }

-- |Register handlers to be invoked when the edit widget has been
-- ''activated'' (when the user presses Enter while the widget is
-- focused).
onActivate :: Widget Edit -> (Widget Edit -> IO ()) -> IO ()
onActivate = addHandler (activateHandlers <~~)

notifyActivateHandlers :: Widget Edit -> IO ()
notifyActivateHandlers wRef = fireEvent wRef (activateHandlers <~~) wRef

notifyChangeHandlers :: Widget Edit -> IO ()
notifyChangeHandlers wRef = do
  s <- getEditText wRef
  fireEvent wRef (changeHandlers <~~) s

notifyCursorMoveHandlers :: Widget Edit -> IO ()
notifyCursorMoveHandlers wRef = do
  pos <- getEditCursorPosition wRef
  fireEvent wRef (cursorMoveHandlers <~~) pos

-- |Register handlers to be invoked when the edit widget's contents
-- change.  Handlers will be passed the new contents.
onChange :: Widget Edit -> (String -> IO ()) -> IO ()
onChange = addHandler (changeHandlers <~~)

-- |Register handlers to be invoked when the edit widget's cursor
-- position changes.  Handlers will be passed the new cursor position,
-- relative to the beginning of the string (position 0).
onCursorMove :: Widget Edit -> (Int -> IO ()) -> IO ()
onCursorMove = addHandler (cursorMoveHandlers <~~)

-- |Get the current contents of the edit widget.
getEditText :: Widget Edit -> IO String
getEditText = (currentText <~~)

-- |Set the contents of the edit widget.
setEditText :: Widget Edit -> String -> IO ()
setEditText wRef str = do
  oldS <- currentText <~~ wRef
  maxLen <- maxTextLength <~~ wRef
  s <- case maxLen of
    Nothing -> return str
    Just l -> return $ take l str
  updateWidgetState wRef $ \st -> st { currentText = s }
  when (oldS /= s) $ do
    gotoBeginning wRef
    notifyChangeHandlers wRef

-- |Set the current edit widget cursor position.  Invalid cursor
-- positions will be ignored.
setEditCursorPosition :: Widget Edit -> Int -> IO ()
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
         notifyCursorMoveHandlers wRef

-- |Get the edit widget's current cursor position.
getEditCursorPosition :: Widget Edit -> IO Int
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
  maxLen <- maxTextLength <~~ wRef
  curLen <- (length . currentText) <~~ wRef
  let proceed = case maxLen of
                  Nothing -> True
                  Just v -> if curLen + 1 > v
                            then False
                            else True

  when proceed $ do
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
