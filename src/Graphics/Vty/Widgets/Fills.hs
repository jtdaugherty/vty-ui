{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Fills
    ( VFill
    , HFill
    , hFill
    , vFill
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( Attr
    , region_width
    , region_height
    , char_fill
    , def_attr
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , RenderContext(..)
    , HasNormalAttr(..)
    , newWidget
    , updateWidget
    , getState
    , updateWidgetState
    )
import Graphics.Vty.Widgets.Util

data VFill = VFill Attr Char
             deriving (Show)

instance HasNormalAttr (Widget VFill) where
    setNormalAttribute w a =
        updateWidgetState w $ \(VFill _ ch) -> VFill a ch

-- |A vertical fill widget.  Fills all available space with the
-- specified character and attribute.
vFill :: (MonadIO m) => Char -> m (Widget VFill)
vFill c = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VFill def_attr c
        , getGrowHorizontal = const $ return False
        , getGrowVertical = const $ return True
        , draw = \this s ctx -> do
                   VFill attr ch <- getState this
                   let attr' = mergeAttrs [ overrideAttr ctx
                                          , attr
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' ch (region_width s) (region_height s)
        }
  return wRef

data HFill = HFill Attr Char Int
             deriving (Show)

instance HasNormalAttr (Widget HFill) where
    setNormalAttribute w a =
        updateWidgetState w $ \(HFill _ ch i) -> HFill a ch i

-- |A horizontal fill widget.  Fills the available horizontal space,
-- one row high, using the specified character and attribute.
hFill :: (MonadIO m) => Char -> Int -> m (Widget HFill)
hFill c h = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HFill def_attr c h
        , getGrowHorizontal = const $ return True
        , getGrowVertical = const $ return False
        , draw = \this s ctx -> do
                   HFill attr ch height <- getState this
                   let attr' = mergeAttrs [ overrideAttr ctx
                                          , attr
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' ch (region_width s) (toEnum height)
        }
  return wRef
