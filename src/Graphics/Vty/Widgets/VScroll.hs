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
            }
    deriving (Show)

infHeight :: Int
infHeight = 1000000

vScroll :: (Show a) => Widget a -> IO (Widget (VScroll a))
vScroll child = do
  let initSt = VScroll 0
  wRef <- newWidget initSt $ \w ->
      w { keyEventHandler = vScrollKeyHandler
        , render_ =
            \this size ctx -> do
              (VScroll cropAmt) <- getState this
              wholeImg <- render child (size `withHeight` infHeight) ctx
              let cropped1 = cropTop (imageHeight wholeImg - cropAmt) wholeImg
                  cropped2 = cropBottom (regionHeight size) cropped1
              return cropped2
        }
  return wRef

vScrollKeyHandler :: Widget (VScroll a) -> Key -> [Modifier] -> IO Bool
vScrollKeyHandler w KUp _ = do
    updateWidgetState w (\(VScroll t) -> VScroll (t - 1))
    return True
vScrollKeyHandler w KDown _ = do
    updateWidgetState w (\(VScroll t) -> VScroll (t + 1))
    return True
vScrollKeyHandler _ _ _ = return False
