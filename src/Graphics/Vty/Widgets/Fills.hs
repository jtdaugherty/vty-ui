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

import Graphics.Vty hiding (regionHeight, regionWidth)
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

data VFill = VFill Char
             deriving (Show)

-- |A vertical fill widget.  Fills all available space with the
-- specified character and attribute.
vFill :: Char -> IO (Widget VFill)
vFill c = do
  wRef <- newWidget (VFill c) $ \w ->
      w { growVertical_ = const $ return True
        , render_ = \this s ctx -> do
                   foc <- focused <~ this
                   VFill ch <- getState this
                   let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                          , normalAttr ctx
                                          ]
                   return $ charFill attr' ch (regionWidth s) (regionHeight s)

        , getCursorPosition_ = const $ return Nothing
        }
  return wRef

data HFill = HFill Char Int
             deriving (Show)

-- |A horizontal fill widget.  Fills the available horizontal space using the
-- specified character. The integer parameter specifies the height, in rows, of
-- the fill.
hFill :: Char -> Int -> IO (Widget HFill)
hFill c h = do
  wRef <- newWidget (HFill c h) $ \w ->
      w { growHorizontal_ = const $ return True
        , render_ = \this s ctx -> do
                   foc <- focused <~ this
                   HFill ch height <- getState this
                   let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                          , normalAttr ctx
                                          ]
                   return $ charFill attr' ch (regionWidth s) (toEnum height)

        , getCursorPosition_ = const $ return Nothing
        }
  return wRef
