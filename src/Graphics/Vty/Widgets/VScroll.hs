module Graphics.Vty.Widgets.VScroll
  ( vScroll
  )
  where

import Graphics.Vty.Image
import Graphics.Vty.Input

import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

data VScroll a =
    VScroll { vScrollTop :: Int
            , vScrollWindow :: Int
            , vLastHeight :: Int
            }
    deriving (Show)

infHeight :: Int
infHeight = 1000000

vScroll :: (Show a) => Widget a -> IO (Widget (VScroll a))
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

vScrollKeyHandler :: Widget (VScroll a) -> Key -> [Modifier] -> IO Bool
vScrollKeyHandler w KPageUp _ = do
    vs <- getState w
    let newST = max (vScrollTop vs - (vScrollWindow vs)) 0
    updateWidgetState w (\vs' -> vs' { vScrollTop = newST })
    return True
vScrollKeyHandler w KPageDown _ = do
    vs <- getState w
    let newST = min (vScrollTop vs + (vScrollWindow vs)) (vLastHeight vs - vScrollWindow vs)
    updateWidgetState w (\vs' -> vs' { vScrollTop = newST })
    return True
vScrollKeyHandler w KUp _ = do
    vs <- getState w
    let newST = vScrollTop vs - 1
    case newST < 0 of
        True -> return ()
        False -> updateWidgetState w (\vs' -> vs' { vScrollTop = newST })
    return True
vScrollKeyHandler w KDown _ = do
    vs <- getState w
    let newST = vScrollTop vs + 1
    case (vLastHeight vs - newST) < vScrollWindow vs of
        True -> return ()
        False -> updateWidgetState w (\vs' -> vs' { vScrollTop = newST })
    return True
vScrollKeyHandler _ _ _ = return False
