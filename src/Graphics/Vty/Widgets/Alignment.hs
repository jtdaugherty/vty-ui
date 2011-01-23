{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Alignment
    ( Alignable(..)
    , Alignment(..)
    , RightAligned
    , rightAligned
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( DisplayRegion(..)
    , (<|>)
    , region_width
    , image_width
    , image_height
    , char_fill
    , empty_image
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , render
    , newWidget
    , updateWidget
    , getState
    , growVertical
    )

data Alignment = AlignCenter | AlignLeft | AlignRight

class Alignable a where
    align :: a -> Alignment -> a

data RightAligned = forall a. RightAligned (Widget a)

rightAligned :: (MonadIO m) => Widget a -> m (Widget RightAligned)
rightAligned chRef = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { getGrowHorizontal = const $ return True
        , getGrowVertical = const $ growVertical chRef
        , state = RightAligned chRef
        , draw =
            \this sz normAttr focAttr mAttr ->
                do
                  RightAligned ch <- getState this
                  img <- render ch sz normAttr focAttr mAttr
                  let diff = region_width sz - image_width img
                      att = maybe normAttr id mAttr
                      fill = if diff > 0
                             then char_fill att ' ' diff (image_height img)
                             else empty_image

                  return $ fill <|> img
        }
  return wRef