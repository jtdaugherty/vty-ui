{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Vty.Widgets.VScroll
  ( vScroll
  )
  where

import Graphics.Vty.Image
import Graphics.Vty.Input

import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Scrollable

data VScroll =
    VScroll { vScrollTop :: Int
            , vScrollWindow :: Int
            , vLastHeight :: Int
            }
    deriving (Show)

infHeight :: Int
infHeight = 1000000

vScroll :: (Show a) => Widget a -> IO (Widget VScroll)
vScroll child = do
  let initSt = VScroll 0 0 0
  wRef <- newWidget initSt $ \w ->
      w { keyEventHandler = vScrollKeyHandler
        , render_ =
            \this size ctx -> do
              wholeImg <- render child (size `withHeight` infHeight) ctx
              updateWidgetState this (\s -> s { vScrollWindow = regionHeight size
                                              , vLastHeight = imageHeight wholeImg
                                              }
                                     )

              st <- getState this
              let cropped1 = cropTop (imageHeight wholeImg - (vScrollTop st)) wholeImg
                  cropped2 = cropBottom (regionHeight size) cropped1
              return cropped2
        }
  return wRef

vScrollKeyHandler :: Widget VScroll -> Key -> [Modifier] -> IO Bool
vScrollKeyHandler w KPageUp _ = pageUp w >> return True
vScrollKeyHandler w KPageDown _ = pageDown w >> return True
vScrollKeyHandler w KUp _ = scrollUp w >> return True
vScrollKeyHandler w KDown _ = scrollDown w >> return True
vScrollKeyHandler _ _ _ = return False

instance Scrollable (Widget VScroll) where
    scrollBy w amt = do
        vs <- getState w
        let newST = min (max (vScrollTop vs + amt) 0) (vLastHeight vs - vScrollWindow vs)
        updateWidgetState w (\vs' -> vs' { vScrollTop = newST })
    pageUp w = do
        vs <- getState w
        scrollBy w (-1 * vScrollWindow vs)
    pageDown w = do
        vs <- getState w
        scrollBy w (vScrollWindow vs)
    scrollToBeginning w =
        updateWidgetState w (\vs -> vs { vScrollTop = 0 })
    scrollToEnd w =
        updateWidgetState w (\vs -> vs { vScrollTop = vLastHeight vs - vScrollWindow vs })
