-- |This module provides high-level composed widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    , boxLimit
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( Attr
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    )
import Graphics.Vty.Widgets.Base
    ( Box
    , VFill
    , vBox
    , vFill
    )
import Graphics.Vty.Widgets.Limits
    ( VLimit
    , HLimit
    , vLimit
    , hLimit
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: (MonadIO m) => Widget a -> Attr -> m (Widget (Box a VFill))
bottomPadded w attr = do
  f <- vFill attr ' '
  vBox w f

-- |Add expanding top padding to a widget.
topPadded :: (MonadIO m) => Widget a -> Attr -> m (Widget (Box VFill a))
topPadded w attr = do
  f <- vFill attr ' '
  vBox f w

-- |Impose a maximum size (width, height) on a widget.
boxLimit :: (MonadIO m) =>
            Int -- ^Maximum width in columns
         -> Int -- ^Maximum height in rows
         -> Widget a
         -> m (Widget (VLimit (HLimit a)))
boxLimit maxWidth maxHeight w = do
  ch <- hLimit maxWidth w
  vLimit maxHeight ch