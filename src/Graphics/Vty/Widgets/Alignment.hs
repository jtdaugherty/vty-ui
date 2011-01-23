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
    , getNormalAttr
    , render
    , newWidget
    , updateWidget
    , getState
    , growVertical
    )

data Alignment = AlignCenter | AlignLeft | AlignRight
                 deriving (Show)

class Alignable a where
    align :: a -> Alignment -> a

data RightAligned = forall a. (Show a) => RightAligned (Widget a)

instance Show RightAligned where
    show _ = "RightAligned { ... }"

rightAligned :: (MonadIO m, Show a) => Widget a -> m (Widget RightAligned)
rightAligned chRef = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { getGrowHorizontal = const $ return True
        , getGrowVertical = const $ growVertical chRef
        , state = RightAligned chRef
        , draw =
            \this sz ctx ->
                do
                  RightAligned ch <- getState this
                  img <- render ch sz ctx
                  let diff = region_width sz - image_width img
                      att = getNormalAttr ctx
                      fill = if diff > 0
                             then char_fill att ' ' diff (image_height img)
                             else empty_image

                  return $ fill <|> img
        }
  return wRef