module Graphics.Vty.Widgets.DirBrowser
    ( DirBrowser(dirBrowserWidget)
    , BrowserSkin(..)
    , newDirBrowser
    , withAnnotations
    , setDirBrowserPath
    , getDirBrowserPath
    , defaultBrowserSkin
    , onBrowseAccept
    , onBrowseCancel
    , onBrowserPathChange
    , reportBrowserError
    , refreshBrowser
    )
where

import Data.IORef
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.List
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Events
import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO.Error

type T = Widget (Box
                 (Box (Box FormattedText FormattedText) HFill)
                 (Box
                  (List [Char] (Box FormattedText FormattedText))
                  (Box
                   (Box (Box FormattedText FormattedText) HFill)
                   FormattedText)))

data DirBrowser = DirBrowser { dirBrowserWidget :: T
                             , dirBrowserList :: Widget (List String (Box FormattedText FormattedText))
                             , dirBrowserPath :: IORef FilePath
                             , dirBrowserPathDisplay :: Widget FormattedText
                             , dirBrowserSelectionMap :: IORef (Map.Map FilePath Int)
                             , dirBrowserFileInfo :: Widget FormattedText
                             , dirBrowserSkin :: BrowserSkin
                             , dirBrowserErrorWidget :: Widget FormattedText
                             , dirBrowserChooseHandlers :: Handlers FilePath
                             , dirBrowserCancelHandlers :: Handlers FilePath
                             , dirBrowserPathChangeHandlers :: Handlers FilePath
                             }

data BrowserSkin = BrowserSkin { browserHeaderAttr :: Attr
                               , browserUnfocusedSelAttr :: Attr
                               , browserErrorAttr :: Attr
                               , browserDirAttr :: Attr
                               , browserLinkAttr :: Attr
                               , browserBlockDevAttr :: Attr
                               , browserNamedPipeAttr :: Attr
                               , browserCharDevAttr :: Attr
                               , browserSockAttr :: Attr
                               , browserCustomAnnotations :: [ (FilePath -> FileStatus -> Bool
                                                               , FilePath -> FileStatus -> IO String
                                                               , Attr)
                                                             ]
                               }

defaultBrowserSkin :: BrowserSkin
defaultBrowserSkin = BrowserSkin { browserHeaderAttr = white `on` blue
                                 , browserUnfocusedSelAttr = bgColor blue
                                 , browserErrorAttr = white `on` red
                                 , browserDirAttr = fgColor green
                                 , browserLinkAttr = fgColor cyan
                                 , browserBlockDevAttr = fgColor red
                                 , browserNamedPipeAttr = fgColor yellow
                                 , browserCharDevAttr = fgColor red
                                 , browserSockAttr = fgColor magenta
                                 , browserCustomAnnotations = []
                                 }

withAnnotations :: BrowserSkin
                -> [(FilePath -> FileStatus -> Bool, FilePath -> FileStatus -> IO String, Attr)]
                -> BrowserSkin
withAnnotations sk as = sk { browserCustomAnnotations = browserCustomAnnotations sk ++ as }

newDirBrowser :: (MonadIO m) => BrowserSkin -> m (DirBrowser, Widget FocusGroup)
newDirBrowser bSkin = do
  path <- liftIO $ getCurrentDirectory
  pathWidget <- plainText ""
  errorText <- plainText "" >>= withNormalAttribute (browserErrorAttr bSkin)
  header <- ((plainText " Path: ") <++> (return pathWidget) <++> (hFill ' ' 1))
            >>= withNormalAttribute (browserHeaderAttr bSkin)

  fileInfo <- plainText ""
  footer <- ((plainText " ") <++> (return fileInfo) <++> (hFill ' ' 1) <++> (return errorText))
            >>= withNormalAttribute (browserHeaderAttr bSkin)

  l <- newList (browserUnfocusedSelAttr bSkin) (\s -> plainText " " <++> plainText s)
  ui <- vBox header =<< vBox l footer

  r <- liftIO $ newIORef ""
  r2 <- liftIO $ newIORef Map.empty

  hs <- newHandlers
  chs <- newHandlers
  pchs <- newHandlers

  let b = DirBrowser { dirBrowserWidget = ui
                     , dirBrowserList = l
                     , dirBrowserPath = r
                     , dirBrowserPathDisplay = pathWidget
                     , dirBrowserSelectionMap = r2
                     , dirBrowserFileInfo = fileInfo
                     , dirBrowserSkin = bSkin
                     , dirBrowserChooseHandlers = hs
                     , dirBrowserCancelHandlers = chs
                     , dirBrowserPathChangeHandlers = pchs
                     , dirBrowserErrorWidget = errorText
                     }

  l `onKeyPressed` handleBrowserKey b
  l `onSelectionChange` (\e -> clearError b >> handleSelectionChange b e)
  b `onBrowserPathChange` setText (dirBrowserPathDisplay b)

  fg <- newFocusGroup
  _ <- addToFocusGroup fg l

  setDirBrowserPath b path
  return (b, fg)

reportBrowserError :: (MonadIO m) => DirBrowser -> String -> m ()
reportBrowserError b msg = setText (dirBrowserErrorWidget b) $ "Error: " ++ msg

clearError :: (MonadIO m) => DirBrowser -> m ()
clearError b = setText (dirBrowserErrorWidget b) ""

onBrowseAccept :: (MonadIO m) => DirBrowser -> (FilePath -> IO ()) -> m ()
onBrowseAccept = addHandler (return . dirBrowserChooseHandlers)

onBrowseCancel :: (MonadIO m) => DirBrowser -> (FilePath -> IO ()) -> m ()
onBrowseCancel = addHandler (return . dirBrowserCancelHandlers)

onBrowserPathChange :: (MonadIO m) => DirBrowser -> (FilePath -> IO ()) -> m ()
onBrowserPathChange = addHandler (return . dirBrowserPathChangeHandlers)

cancelBrowse :: (MonadIO m) => DirBrowser -> m ()
cancelBrowse b = fireEvent b (return . dirBrowserCancelHandlers) =<< getDirBrowserPath b

chooseCurrentEntry :: (MonadIO m) => DirBrowser -> m ()
chooseCurrentEntry b = do
  p <- getDirBrowserPath b
  mCur <- getSelected (dirBrowserList b)
  case mCur of
    Nothing -> return ()
    Just (_, (e, _)) -> fireEvent b (return . dirBrowserChooseHandlers) (p </> e)

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
  (_, mkAnn) <- fileAnnotation (dirBrowserSkin b) st cur path
  ann <- liftIO mkAnn
  return $ path ++ ": " ++ ann

builtInAnnotations :: FilePath -> BrowserSkin -> [(FilePath -> FileStatus -> Bool, FilePath -> FileStatus -> IO String, Attr)]
builtInAnnotations cur sk =
    [ (\_ s -> isRegularFile s
      , \_ s -> return $ "regular file, " ++
                (show $ fileSize s) ++ " bytes"
      , def_attr)
    , (\_ s -> isSymbolicLink s,
       (\p stat -> do
          linkDest <- if not $ isSymbolicLink stat
                      then return ""
                      else do
                        linkPath <- liftIO $ readSymbolicLink p
                        liftIO $ canonicalizePath $ cur </> linkPath
          return $ "symbolic link to " ++ linkDest)
      , browserLinkAttr sk)
    , (\_ s -> isDirectory s, \_ _ -> return "directory", browserDirAttr sk)
    , (\_ s -> isBlockDevice s, \_ _ -> return "block device", browserBlockDevAttr sk)
    , (\_ s -> isNamedPipe s, \_ _ -> return "named pipe", browserNamedPipeAttr sk)
    , (\_ s -> isCharacterDevice s, \_ _ -> return "character device", browserCharDevAttr sk)
    , (\_ s -> isSocket s, \_ _ -> return "socket", browserSockAttr sk)
    ]

fileAnnotation :: (MonadIO m) => BrowserSkin -> FileStatus -> FilePath -> FilePath -> m (Attr, IO String)
fileAnnotation sk st cur shortPath = do
  let fullPath = cur </> shortPath

      annotation = getAnnotation' fullPath st $ (browserCustomAnnotations sk) ++
                   (builtInAnnotations cur sk)

      getAnnotation' _ _ [] = (def_attr, return "")
      getAnnotation' pth stat ((f,mkAnn,a):rest) =
          if f pth stat
          then (a, mkAnn pth stat)
          else getAnnotation' pth stat rest

  return annotation

handleBrowserKey :: DirBrowser -> Widget (List a b) -> Key -> [Modifier] -> IO Bool
handleBrowserKey b _ KEnter [] = descend b True >> return True
handleBrowserKey b _ KRight [] = descend b False >> return True
handleBrowserKey b _ KLeft [] = ascend b >> return True
handleBrowserKey b _ KEsc [] = cancelBrowse b >> return True
handleBrowserKey b _ (KASCII 'q') [] = cancelBrowse b >> return True
handleBrowserKey b _ (KASCII 'r') [] = refreshBrowser b >> return True
handleBrowserKey _ _ _ _ = return False

refreshBrowser :: (MonadIO m) => DirBrowser -> m ()
refreshBrowser b = setDirBrowserPath b =<< getDirBrowserPath b

setDirBrowserPath :: (MonadIO m) => DirBrowser -> FilePath -> m ()
setDirBrowserPath b path = do
  cPath <- liftIO $ canonicalizePath path

  -- If for some reason we can't load the directory, report an error
  -- and don't change the browser state.
  (res, entries) <-
      liftIO $ (do
                 entries <- getDirectoryContents cPath
                 return (True, entries))
                `catch` \e -> do
                             reportBrowserError b (ioeGetErrorString e)
                             return (False, [])

  when res $ do
    -- If something is currently selected, store that in the selection
    -- map before changing the path.
    cur <- getDirBrowserPath b
    mCur <- getSelected (dirBrowserList b)
    case mCur of
      Nothing -> return ()
      Just (i, _) -> storeSelection b cur i

    clearList (dirBrowserList b)
    liftIO $ modifyIORef (dirBrowserPath b) $ const cPath

    liftIO $ load b cPath entries

    sel <- getSelection b path
    case sel of
      Nothing -> return ()
      Just i -> scrollBy (dirBrowserList b) i

    fireEvent b (return . dirBrowserPathChangeHandlers) cPath

getDirBrowserPath :: (MonadIO m) => DirBrowser -> m FilePath
getDirBrowserPath = liftIO . readIORef . dirBrowserPath

storeSelection :: (MonadIO m) => DirBrowser -> FilePath -> Int -> m ()
storeSelection b path i =
    liftIO $ modifyIORef (dirBrowserSelectionMap b) $ \m -> Map.insert path i m

getSelection :: (MonadIO m) => DirBrowser -> FilePath -> m (Maybe Int)
getSelection b path =
    liftIO $ do
      st <- readIORef (dirBrowserSelectionMap b)
      return $ Map.lookup path st

load :: DirBrowser -> FilePath -> [FilePath] -> IO ()
load b cur entries =
    forM_ entries $ \entry -> do
      let fullPath = cur </> entry
      f <- getSymbolicLinkStatus fullPath
      (attr, _) <- fileAnnotation (dirBrowserSkin b) f cur entry
      (_, w) <- addToList (dirBrowserList b) entry
      ch <- getSecondChild w
      setNormalAttribute ch attr

descend :: (MonadIO m) => DirBrowser -> Bool -> m ()
descend b shouldSelect = do
  base <- getDirBrowserPath b
  mCur <- getSelected (dirBrowserList b)
  case mCur of
    Nothing -> return ()
    Just (_, (p, _)) -> do
              let newPath = base </> p
              e <- liftIO $ doesDirectoryExist newPath
              case e of
                True -> do
                       cPath <- liftIO $ canonicalizePath newPath
                       cur <- getDirBrowserPath b
                       when (cur /= cPath) $ do
                          case takeDirectory cur == cPath of
                            True -> ascend b
                            False -> setDirBrowserPath b cPath

                False -> when shouldSelect $ chooseCurrentEntry b

ascend :: (MonadIO m) => DirBrowser -> m ()
ascend b = do
  cur <- liftIO $ getDirBrowserPath b
  let newPath = takeDirectory cur
  when (newPath /= cur) $
       setDirBrowserPath b newPath