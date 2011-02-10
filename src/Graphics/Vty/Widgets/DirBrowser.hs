module Graphics.Vty.Widgets.DirBrowser
    ( DirBrowser(dirBrowserWidget, dirBrowserList)
    , BrowserSkin(..)
    , newDirBrowser
    , setDirBrowserPath
    , getDirBrowserPath
    , defaultBrowserSkin
    )
where

import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.List
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Util
import System.Directory
import System.FilePath
import System.Posix.Files

type T = Widget (Box
                  (Box (Box FormattedText FormattedText) HFill)
                  (Box
                   (List String FormattedText)
                   (Box
                    (Box FormattedText FormattedText) HFill)))

data DirBrowser = DirBrowser { dirBrowserWidget :: T
                             , dirBrowserList :: Widget (List String FormattedText)
                             , dirBrowserPath :: IORef FilePath
                             , dirBrowserPathDisplay :: Widget FormattedText
                             , dirBrowserSelectionStack :: IORef [Int]
                             , dirBrowserFileInfo :: Widget FormattedText
                             , dirBrowserSkin :: BrowserSkin
                             }

data BrowserSkin = BrowserSkin { browserHeaderAttr :: Attr
                               , browserUnfocusedSelAttr :: Attr
                               , browserDirAttr :: Attr
                               , browserLinkAttr :: Attr
                               }

defaultBrowserSkin :: BrowserSkin
defaultBrowserSkin = BrowserSkin { browserHeaderAttr = white `on` blue
                                 , browserUnfocusedSelAttr = bgColor blue
                                 , browserDirAttr = fgColor green
                                 , browserLinkAttr = fgColor cyan
                                 }

newDirBrowser :: (MonadIO m) => FilePath -> BrowserSkin -> m DirBrowser
newDirBrowser path bSkin = do
  pathWidget <- simpleText ""
  header <- ((simpleText " Path: ") <++> (return pathWidget) <++> (hFill ' ' 1))
            >>= withNormalAttribute (browserHeaderAttr bSkin)

  fileInfo <- simpleText ""
  footer <- ((simpleText " File info: ") <++> (return fileInfo) <++> (hFill ' ' 1))
            >>= withNormalAttribute (browserHeaderAttr bSkin)

  l <- newListWidget =<< newList (browserUnfocusedSelAttr bSkin) (simpleText . (" " ++))
  ui <- vBox header =<< vBox l footer

  r <- liftIO $ newIORef ""
  r2 <- liftIO $ newIORef []

  let b = DirBrowser { dirBrowserWidget = ui
                     , dirBrowserList = l
                     , dirBrowserPath = r
                     , dirBrowserPathDisplay = pathWidget
                     , dirBrowserSelectionStack = r2
                     , dirBrowserFileInfo = fileInfo
                     , dirBrowserSkin = bSkin
                     }

  l `onKeyPressed` handleBrowserKey b
  l `onSelectionChange` handleSelectionChange b

  setDirBrowserPath b path
  return b

handleSelectionChange :: DirBrowser -> SelectionEvent String b -> IO ()
handleSelectionChange b ev = do
  case ev of
    SelectionOff -> setText (dirBrowserFileInfo b) "-"
    SelectionOn _ path _ -> setText (dirBrowserFileInfo b) =<< getFileInfo b path

getFileInfo :: (MonadIO m) => DirBrowser -> FilePath -> m String
getFileInfo b path = do
  cur <- getDirBrowserPath b
  let newPath = cur </> path

  st <- liftIO $ getSymbolicLinkStatus newPath
  linkDest <- if not $ isSymbolicLink st
              then return ""
              else do
                linkPath <- liftIO $ readSymbolicLink newPath
                liftIO $ canonicalizePath $ cur </> linkPath

  return $ fileInfoStr st [ (isRegularFile, \s -> "regular file, " ++
                                                  (show $ fileSize s) ++ " bytes")
                          , (isSymbolicLink, const $ "symbolic link to " ++ linkDest)
                          , (isDirectory, const "directory")
                          ]

fileInfoStr :: FileStatus -> [(FileStatus -> Bool, FileStatus -> String)] -> String
fileInfoStr _ [] = ""
fileInfoStr st ((f,info):rest) = if f st then info st else fileInfoStr st rest

handleBrowserKey :: DirBrowser -> Widget (List a b) -> Key -> [Modifier] -> IO Bool
handleBrowserKey b _ KEnter [] = descend b >> return True
handleBrowserKey b _ KRight [] = descend b >> return True
handleBrowserKey b _ KLeft [] = ascend b >> return True
handleBrowserKey _ _ _ _ = return False

setDirBrowserPath :: (MonadIO m) => DirBrowser -> FilePath -> m ()
setDirBrowserPath b path = do
  clearList (dirBrowserList b)
  cPath <- liftIO $ canonicalizePath path
  liftIO $ modifyIORef (dirBrowserPath b) $ const cPath
  setText (dirBrowserPathDisplay b) cPath
  load b

getDirBrowserPath :: (MonadIO m) => DirBrowser -> m FilePath
getDirBrowserPath = liftIO . readIORef . dirBrowserPath

pushSelection :: (MonadIO m) => DirBrowser -> Int -> m ()
pushSelection b i =
    liftIO $ modifyIORef (dirBrowserSelectionStack b) $ \s -> s ++ [i]

popSelection :: (MonadIO m) => DirBrowser -> m (Maybe Int)
popSelection b =
    liftIO $ do
      st <- readIORef (dirBrowserSelectionStack b)
      case st of
        [] -> return Nothing
        es -> do
               let (rest, v) = (init es, last es)
               writeIORef (dirBrowserSelectionStack b) rest
               return $ Just v

load :: (MonadIO m) => DirBrowser -> m ()
load b = do
  cur <- getDirBrowserPath b
  entries <- liftIO (getDirectoryContents cur)
  forM_ entries $ \entry -> do
           f <- liftIO $ getSymbolicLinkStatus $ cur </> entry
           let attr = if isRegularFile f
                      then def_attr
                      else if isSymbolicLink f
                           then browserLinkAttr (dirBrowserSkin b)
                           else if isDirectory f
                                then browserDirAttr (dirBrowserSkin b)
                                else def_attr
           (_, w) <- addToList (dirBrowserList b) (entry)
           setNormalAttribute w attr

descend :: (MonadIO m) => DirBrowser -> m ()
descend b = do
  base <- getDirBrowserPath b
  mCur <- getSelected (dirBrowserList b)
  case mCur of
    Nothing -> return ()
    Just (i, (p, _)) -> do
              let newPath = base </> p
              e <- liftIO $ doesDirectoryExist newPath
              case e of
                True -> do
                       cPath <- liftIO $ canonicalizePath newPath
                       cur <- getDirBrowserPath b
                       when (cur /= cPath) $ do
                          case takeDirectory cur == cPath of
                            True -> ascend b
                            False -> do
                              pushSelection b i
                              setDirBrowserPath b cPath

                False -> do
                          -- XXX send activation signal for file since
                          -- this isn't a directory
                          return ()

ascend :: (MonadIO m) => DirBrowser -> m ()
ascend b = do
  cur <- liftIO $ getDirBrowserPath b
  let newPath = takeDirectory cur
  when (newPath /= cur) $ do
             -- Set the new path.
             setDirBrowserPath b newPath

             -- Pop a selection from the selection stack and update
             -- the list selection.
             res <- popSelection b
             case res of
               Nothing -> return ()
               Just i -> scrollBy (dirBrowserList b) i