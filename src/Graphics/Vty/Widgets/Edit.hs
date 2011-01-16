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
    ( when
    )
import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( Attr
    , Key(..)
    , Modifier(..)
    , (<|>)
    , region_width
    , string
    , char_fill
    , with_style
    , underline
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , (<~)
    , (<~~)
    , getPhysicalPosition
    , withWidth
    , updateWidget
    , updateWidgetState
    , newWidget
    , getState
    )

data Edit = Edit { currentText :: String
                 , cursorPosition :: Int
                 , normalAttr :: Attr
                 , focusAttr :: Attr
                 , displayStart :: Int
                 , displayWidth :: Int
                 , activateHandler :: Widget Edit -> IO ()
                 , changeHandler :: Widget Edit -> String -> IO ()
                 , cursorMoveHandler :: Widget Edit -> Int -> IO ()
                 }

editWidget :: (MonadIO m) => Attr -> Attr -> m (Widget Edit)
editWidget normAtt focAtt = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Edit { currentText = ""
                       , cursorPosition = 0
                       , normalAttr = normAtt `with_style` underline
                       , focusAttr = focAtt `with_style` underline
                       , displayStart = 0
                       , displayWidth = 0
                       , activateHandler = const $ return ()
                       , changeHandler = \_ _ -> return ()
                       , cursorMoveHandler = \_ _ -> return ()
                       }

        , getGrowHorizontal = return True
        , getGrowVertical = return False
        , cursorInfo =
            \this -> do
              f <- focused <~ this
              pos <- getPhysicalPosition this
              curPos <- cursorPosition <~~ this
              start <- displayStart <~~ this

              if f then
                  return (Just $ pos `withWidth` ((region_width pos) + toEnum (curPos - start))) else
                  return Nothing

        , draw =
            \this size _ -> do
              setDisplayWidth this (fromEnum $ region_width size)
              st <- getState this

              let truncated = take (displayWidth st)
                              (drop (displayStart st) (currentText st))

              isFocused <- focused <~ this
              let attr = if isFocused then focusAttr st else normalAttr st
              return $ string attr truncated
                         <|> char_fill attr ' ' (region_width size - (toEnum $ length truncated)) 1

        , keyEventHandler = editKeyEvent
        }
  return wRef

onActivate :: Widget Edit -> (Widget Edit -> IO ()) -> IO ()
onActivate wRef handler = do
  oldHandler <- activateHandler <~~ wRef

  let combinedHandler =
          \w -> do
            oldHandler w
            handler w

  updateWidgetState wRef $ \s -> s { activateHandler = combinedHandler }

onChange :: Widget Edit -> (Widget Edit -> String -> IO ()) -> IO ()
onChange wRef handler = do
  oldHandler <- changeHandler <~~ wRef

  let combinedHandler =
          \w str -> do
            oldHandler w str
            handler w str

  updateWidgetState wRef $ \s -> s { changeHandler = combinedHandler }

onCursorMove :: Widget Edit -> (Widget Edit -> Int -> IO ()) -> IO ()
onCursorMove wRef handler = do
  oldHandler <- cursorMoveHandler <~~ wRef

  let combinedHandler =
          \w pos -> do
            oldHandler w pos
            handler w pos

  updateWidgetState wRef $ \s -> s { cursorMoveHandler = combinedHandler }

getEditText :: Widget Edit -> IO String
getEditText = (currentText <~~)

setEditText :: Widget Edit -> String -> IO ()
setEditText wRef str = do
  updateWidgetState wRef $ \s -> s { currentText = str }
  gotoBeginning wRef
  notifyChangeHandler wRef

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
         notifyCursorMoveHandler wRef

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
    (KEnter, []) -> notifyActivateHandler this >> return True
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

  notifyChangeHandler this

deletePreviousChar :: Widget Edit -> IO ()
deletePreviousChar this = do
  pos <- cursorPosition <~~ this
  when (pos /= 0) $ do
    moveCursorLeft this
    delCurrentChar this

notifyChangeHandler :: Widget Edit -> IO ()
notifyChangeHandler wRef = do
  h <- changeHandler <~~ wRef
  str <- getEditText wRef
  h wRef str

notifyCursorMoveHandler :: Widget Edit -> IO ()
notifyCursorMoveHandler wRef = do
  h <- cursorMoveHandler <~~ wRef
  pos <- getEditCursorPosition wRef
  h wRef pos

notifyActivateHandler :: Widget Edit -> IO ()
notifyActivateHandler wRef = do
  h <- activateHandler <~~ wRef
  h wRef

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
      notifyCursorMoveHandler wRef

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
         notifyCursorMoveHandler wRef

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
  notifyChangeHandler wRef

delCurrentChar :: Widget Edit -> IO ()
delCurrentChar wRef = do
  st <- getState wRef
  when (cursorPosition st < (length $ currentText st)) $
       do
         let newContent = remove (cursorPosition st) (currentText st)
         updateWidgetState wRef $ \s -> s { currentText = newContent }
         notifyChangeHandler wRef

remove :: Int -> [a] -> [a]
remove pos as = (take pos as) ++ (drop (pos + 1) as)

inject :: Int -> a -> [a] -> [a]
inject pos a as = let (h, t) = splitAt pos as
                  in h ++ (a:t)
