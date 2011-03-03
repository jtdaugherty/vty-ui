{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- |This module provides ''space-filling'' widgets used to control
-- layout.
module Graphics.Vty.Widgets.Fills
    ( VFill
    , HFill
    , hFill
    , vFill
    )
where

import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

data VFill = VFill Char
             deriving (Show)

-- |A vertical fill widget.  Fills all available space with the
-- specified character and attribute.
vFill :: (MonadIO m) => Char -> m (Widget VFill)
vFill c = do
  wRef <- newWidget $ \w ->
      w { state = VFill c
        , growVertical_ = const $ return True
        , render_ = \this s ctx -> do
                   foc <- focused <~ this
                   VFill ch <- getState this
                   let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' ch (region_width s) (region_height s)
        }
  return wRef

data HFill = HFill Char Int
             deriving (Show)

-- |A horizontal fill widget.  Fills the available horizontal space,
-- one row high, using the specified character and attribute.
hFill :: (MonadIO m) => Char -> Int -> m (Widget HFill)
hFill c h = do
  wRef <- newWidget $ \w ->
      w { state = HFill c h
        , growHorizontal_ = const $ return True
        , render_ = \this s ctx -> do
                   foc <- focused <~ this
                   HFill ch height <- getState this
                   let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' ch (region_width s) (toEnum height)
        }
  return wRef
