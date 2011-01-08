module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
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
    , (<|>)
    , region_width
    , string
    , char_fill
    )
import Graphics.Vty.Widgets.Rendering
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
                 }

editWidget :: (MonadIO m) => Attr -> Attr -> String -> m (Widget Edit)
editWidget normAtt focAtt str = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Edit { currentText = str
                       , cursorPosition = 0
                       , normalAttr = normAtt
                       , focusAttr = focAtt
                       }

        , getGrowHorizontal = return True
        , getGrowVertical = return False
        , cursorInfo =
            \this -> do
              f <- focused <~ this
              pos <- getPhysicalPosition this
              curPos <- cursorPosition <~~ this
              if f then
                  return (Just $ pos `withWidth` ((region_width pos) + toEnum curPos)) else
                  return Nothing

        , draw =
            \this size _ -> do
              st <- getState this
              isFocused <- focused <~ this
              let attr = if isFocused then focusAttr st else normalAttr st
              return $ string attr (currentText st)
                         <|> char_fill attr ' ' (region_width size - (toEnum $ length $ currentText st)) 1

        , keyEventHandler = editKeyEvent
        }

editKeyEvent :: Widget Edit -> Key -> IO Bool
editKeyEvent this k = do
  case k of
    KLeft -> moveCursorLeft this >> return True
    KRight -> moveCursorRight this >> return True
    KBS -> do
           pos <- cursorPosition <~~ this
           when (pos /= 0) $ do
                        moveCursorLeft this
                        delCurrentChar this
           return True
    KDel -> delCurrentChar this >> return True
    (KASCII ch) -> insertChar this ch >> moveCursorRight this >> return True
    KHome -> cursorHome this >> return True
    KEnd -> cursorEnd this >> return True
    _ -> return False

moveCursorLeft :: Widget Edit -> IO ()
moveCursorLeft wRef = do
  st <- getState wRef
  case cursorPosition st of
    0 -> return ()
    p -> updateWidgetState_ wRef $ \s -> s { cursorPosition = p - 1 }

moveCursorRight :: Widget Edit -> IO ()
moveCursorRight wRef = do
  st <- getState wRef
  when (cursorPosition st < (length $ currentText st)) $
       updateWidgetState_ wRef $ \s -> s { cursorPosition = (cursorPosition st) + 1 }

cursorHome :: Widget Edit -> IO ()
cursorHome wRef = updateWidgetState_ wRef $ \st -> st { cursorPosition = 0 }

cursorEnd :: Widget Edit -> IO ()
cursorEnd wRef = updateWidgetState_ wRef $ \st ->
                 st { cursorPosition = length (currentText st) }

insertChar :: Widget Edit -> Char -> IO ()
insertChar wRef ch = do
  updateWidgetState_ wRef $ \st ->
      let (begin, end) = ( take (cursorPosition st) $ currentText st
                         , drop (cursorPosition st) $ currentText st
                         )
      in st { currentText = begin ++ [ch] ++ end }

delCurrentChar :: Widget Edit -> IO ()
delCurrentChar wRef = do
  st <- getState wRef
  when (cursorPosition st < (length $ currentText st)) $
       updateWidgetState_ wRef $ \s ->
           let (begin, end) = ( take (cursorPosition st) $ currentText st
                              , drop (cursorPosition st + 1) $ currentText st
                              )
           in s { currentText = begin ++ end }