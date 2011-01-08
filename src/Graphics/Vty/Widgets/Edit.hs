module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
    , getEditText
    , setEditText
    , onActivate
    , onChange
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
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , (<~)
    , (<~~)
    , getPhysicalPosition
    , withWidth
    , updateWidget
    , updateWidgetState_
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
                 , changeHandler :: Widget Edit -> IO ()
                 }

editWidget :: (MonadIO m) => Attr -> Attr -> m (Widget Edit)
editWidget normAtt focAtt = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Edit { currentText = ""
                       , cursorPosition = 0
                       , normalAttr = normAtt
                       , focusAttr = focAtt
                       , displayStart = 0
                       , displayWidth = 0
                       , activateHandler = const $ return ()
                       , changeHandler = const $ return ()
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

onActivate :: Widget Edit -> (Widget Edit -> IO ()) -> IO ()
onActivate wRef handler = do
  oldHandler <- activateHandler <~~ wRef

  let combinedHandler =
          \w -> do
            oldHandler w
            handler w

  updateWidgetState_ wRef $ \s -> s { activateHandler = combinedHandler }

onChange :: Widget Edit -> (Widget Edit -> IO ()) -> IO ()
onChange wRef handler = do
  oldHandler <- changeHandler <~~ wRef

  let combinedHandler =
          \w -> do
            oldHandler w
            handler w

  updateWidgetState_ wRef $ \s -> s { changeHandler = combinedHandler }

getEditText :: Widget Edit -> IO String
getEditText = (currentText <~~)

setEditText :: Widget Edit -> String -> IO ()
setEditText wRef str = do
  updateWidgetState_ wRef $ \s -> s { currentText = str }
  gotoBeginning wRef
  notifyChangeHandler wRef

setDisplayWidth :: Widget Edit -> Int -> IO ()
setDisplayWidth this width =
    updateWidgetState_ this $ \s ->
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
    (KASCII 'e', [MCtrl]) -> gotoEnd this >> return True
    (KLeft, []) -> moveCursorLeft this >> return True
    (KRight, []) -> moveCursorRight this >> return True
    (KBS, []) -> do
           pos <- cursorPosition <~~ this
           when (pos /= 0) $ do
                        moveCursorLeft this
                        delCurrentChar this
                        notifyChangeHandler this
           return True
    (KDel, []) -> delCurrentChar this >> notifyChangeHandler this >> return True
    (KASCII ch, []) -> insertChar this ch >> moveCursorRight this >> notifyChangeHandler this >> return True
    (KHome, []) -> cursorHome this >> return True
    (KEnd, []) -> cursorEnd this >> return True
    (KEnter, []) -> notifyActivateHandler this >> return True
    _ -> return False

notifyChangeHandler :: Widget Edit -> IO ()
notifyChangeHandler wRef = do
  h <- changeHandler <~~ wRef
  h wRef

notifyActivateHandler :: Widget Edit -> IO ()
notifyActivateHandler wRef = do
  h <- activateHandler <~~ wRef
  h wRef

gotoBeginning :: Widget Edit -> IO ()
gotoBeginning wRef = do
  updateWidgetState_ wRef $ \s -> s { displayStart = 0
                                    , cursorPosition = 0
                                    }

gotoEnd :: Widget Edit -> IO ()
gotoEnd wRef = do
  updateWidgetState_ wRef $ \s ->
      s { displayStart = if (length $ currentText s) > displayWidth s
                         then (length $ currentText s) - displayWidth s
                         else 0
        , cursorPosition = length $ currentText s
        }

moveCursorLeft :: Widget Edit -> IO ()
moveCursorLeft wRef = do
  st <- getState wRef

  case cursorPosition st of
    0 -> return ()
    p -> do
      let newDispStart = if p == displayStart st
                         then displayStart st - 1
                         else displayStart st
      updateWidgetState_ wRef $ \s ->
          s { cursorPosition = p - 1
            , displayStart = newDispStart
            }

moveCursorRight :: Widget Edit -> IO ()
moveCursorRight wRef = do
  st <- getState wRef

  when (cursorPosition st < (length $ currentText st)) $
       do
         let newDispStart = if cursorPosition st == displayStart st + displayWidth st - 1
                            then displayStart st + 1
                            else displayStart st
         updateWidgetState_ wRef $ \s ->
             s { cursorPosition = cursorPosition st + 1
               , displayStart = newDispStart
               }

cursorHome :: Widget Edit -> IO ()
cursorHome wRef = updateWidgetState_ wRef $ \st -> st { cursorPosition = 0 }

cursorEnd :: Widget Edit -> IO ()
cursorEnd wRef = updateWidgetState_ wRef $ \st ->
                 st { cursorPosition = length (currentText st) }

insertChar :: Widget Edit -> Char -> IO ()
insertChar wRef ch = do
  updateWidgetState_ wRef $ \st ->
      let newContent = inject (cursorPosition st) ch (currentText st)
          newViewStart =
              if cursorPosition st == displayStart st + displayWidth st - 1
              then displayStart st + 1
              else displayStart st
      in st { currentText = newContent
            , displayStart = newViewStart
            }

delCurrentChar :: Widget Edit -> IO ()
delCurrentChar wRef = do
  st <- getState wRef
  when (cursorPosition st < (length $ currentText st)) $
       do
         let newContent = remove (cursorPosition st) (currentText st)
         updateWidgetState_ wRef $ \s -> s { currentText = newContent }

remove :: Int -> [a] -> [a]
remove pos as = (take pos as) ++ (drop (pos + 1) as)

inject :: Int -> a -> [a] -> [a]
inject pos a as = let (h, t) = splitAt pos as
                  in h ++ (a:t)
