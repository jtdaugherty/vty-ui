module Main where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import System.Exit
import System

import Graphics.Vty
import Graphics.Vty.Widgets.All

type T = (Box
          (Box HBorder (List String Edit))
          (Box (Box
                (HFixed HBorder)
                (Box (Box (Box
                           (Box
                            (Box
                             (Box (Box FormattedText FormattedText) FormattedText)
                             FormattedText)
                            FormattedText)
                           FormattedText)
                      FormattedText)
                 FormattedText))
           HBorder))

data Editor =
   Editor { editorWidget :: Widget T
          , editorList :: Widget (List String Edit)
          , cursorColumn :: IORef Int
          , curLine :: Widget FormattedText
          , curCol :: Widget FormattedText
          , totalLines :: Widget FormattedText
          }

setCursorColumn :: (MonadIO m) => Editor -> Int -> m ()
setCursorColumn e col = do
  liftIO $ writeIORef (cursorColumn e) col
  updateCurColumn e col

updateTotalLines :: (MonadIO m) => Editor -> m ()
updateTotalLines e = do
  sz <- getListSize (editorList e)
  setText (totalLines e) $ show sz

updateCurLine :: (MonadIO m) => Editor -> m ()
updateCurLine e = do
  cur <- getSelected (editorList e)
  case cur of
    Nothing -> setText (curLine e) "--"
    Just (pos, _) -> setText (curLine e) $ show (pos + 1)

updateCurColumn :: (MonadIO m) => Editor -> Int -> m ()
updateCurColumn e col = setText (curCol e) (show $ col + 1)

backspaceLineCollapse :: (MonadIO m) => Editor -> Widget Edit -> m Bool
backspaceLineCollapse e w = do
  s <- getEditText w
  cp <- getEditCursorPosition w

  when (null s) $ do
    Just (pos, _) <- getSelected (editorList e)
    _ <- removeFromList (editorList e) pos
    scrollUp (editorList e)

    -- Once the entry has been deleted, go to the end of the
    -- newly-selected entry.
    Just (_, (_, w')) <- getSelected (editorList e)
    s' <- getEditText w'
    setEditCursorPosition w' (length s')

  when ((not $ null s) && (cp == 0)) $ do
    -- Collapse the current line with the previous one and set the
    -- cursor position as appropriate.
    Just (pos, _) <- getSelected (editorList e)
    _ <- removeFromList (editorList e) pos

    -- Only scroll if this isn't the last line, since the List takes
    -- care of that.
    sz <- getListSize (editorList e)
    when (pos < sz - 1) $
         scrollUp (editorList e)

    Just (_, (_, w')) <- getSelected (editorList e)
    s' <- getEditText w'
    setEditText w' (s' ++ s)
    setEditCursorPosition w' (length s')

  return False

delLineCollapse :: (MonadIO m) => Editor -> Widget Edit -> m Bool
delLineCollapse e w = do
  s <- getEditText w
  cp <- getEditCursorPosition w
  sz <- getListSize (editorList e)
  Just (pos, _) <- getSelected (editorList e)

  case (cp == length s && pos < sz - 1) of
    False -> return False
    True -> do
      -- Get the content of the next line, if any, and move it to this
      -- one.
      (_, w') <- removeFromList (editorList e) (pos + 1)
      s' <- getEditText w'
      setEditText w (s ++ s')
      setEditCursorPosition w (length s)
      return True

setupEditWidget :: (MonadIO m) => Editor -> Widget Edit -> m ()
setupEditWidget e w = do
  w `onCursorMove` setCursorColumn e
  w `onKeyPressed` \_ k mods ->
      case (k, mods) of
        (KBS, []) -> backspaceLineCollapse e w
        (KASCII 'd', [MCtrl]) -> delLineCollapse e w
        (KASCII 'k', [MCtrl]) -> do
                        s <- getEditText w
                        case null s of
                          True -> delLineCollapse e w
                          False -> return False
        (KDel, []) -> delLineCollapse e w
        _ -> return False

  -- We don't want the default underline style that Edit uses, so we
  -- turn it off.
  setNormalAttribute w $ style default_style_mask
  setFocusAttribute w $ style default_style_mask

loadDocument :: (MonadIO m) => Editor -> String -> m ()
loadDocument e doc = do
  clearList (editorList e)
  forM_ (lines doc) $ addToList (editorList e)

getDocument :: (MonadIO m) => Editor -> m String
getDocument e = do
  len <- getListSize (editorList e)
  ls <- forM [0..len-1] $ \i ->
        do
          Just (_, w) <- getListItem (editorList e) i
          getEditText w
  return $ unlines ls

newEditor :: (MonadIO m) => m (Editor, Widget FocusGroup)
newEditor = do
  lst <- newList def_attr $ \s -> do
              w <- editWidget
              setEditText w s
              return w

  lst `onKeyPressed` \_ k mods ->
      case (mods, k) of
        ([MCtrl], KASCII 'v') -> pageDown lst >> return True
        ([MMeta], KASCII 'v') -> pageUp lst >> return True
        ([MMeta], KASCII '>') -> do
                       sz <- getListSize lst
                       setSelected lst sz
                       return True
        ([MMeta], KASCII '<') -> do
                       setSelected lst 0
                       return True
        _ -> return False

  lst `onItemActivated` \(ActivateItemEvent pos _ w) ->
      do
        s <- getEditText w
        curPos <- getEditCursorPosition w
        setEditText w (drop curPos s)
        (_, w') <- insertIntoList lst (take curPos s) pos
        setEditCursorPosition w' 0

  curLineW <- plainText "1"
  curColW <- plainText "1"
  totalLinesW <- plainText "0"

  lineDisplay <- (plainText " [L") <++> (return curLineW) <++> (plainText "/")
                 <++> (return totalLinesW) <++> (plainText " ") <++> (plainText "C")
                <++> (return curColW) <++> (plainText "] ")

  footer <- (hFixed 3 =<< (hBorder >>= withBorderAttribute (fgColor green)))
            <++> (return lineDisplay)
           <++> (hBorder >>= withBorderAttribute (fgColor green))

  ui <- (hBorder >>= withBorderAttribute (fgColor green))
            <--> (return lst)
            <--> (return footer >>= withNormalAttribute (fgColor yellow))

  cRef <- liftIO $ newIORef 0
  let e = Editor ui lst cRef curLineW curColW totalLinesW

  lst `onItemAdded` \(NewItemEvent _ _ w) -> updateTotalLines e >> setupEditWidget e w
  lst `onItemRemoved` \_ -> updateTotalLines e

  lst `onSelectionChange` \ev ->
      case ev of
        SelectionOff -> updateCurLine e
        SelectionOn _ _ w -> do
                updateCurLine e
                cp <- getEditCursorPosition w
                updateCurColumn e cp
                (readIORef $ cursorColumn e)
                >>= setEditCursorPosition w

  fg <- newFocusGroup
  _ <- addToFocusGroup fg lst
  return (e, fg)

main :: IO ()
main = do
  args <- getArgs
  n <- getProgName

  when (length args /= 1) $ do
         putStrLn $ "Usage: " ++ n ++ " <filename>"
         exitFailure

  (e, fg) <- newEditor
  loadDocument e =<< readFile (args !! 0)

  c <- newCollection
  _ <- addToCollection c (editorWidget e) fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> exitSuccess
        _ -> return False

  runUi c defaultContext
