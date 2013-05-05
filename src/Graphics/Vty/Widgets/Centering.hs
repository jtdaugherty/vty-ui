{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides widgets to center other widgets horizontally
-- and vertically.  These centering widgets relay focus and key events
-- to their children.
module Graphics.Vty.Widgets.Centering
    ( HCentered
    , VCentered
    , hCentered
    , vCentered
    , centered
    )
where

import GHC.Word ( Word )
import Graphics.Vty.Widgets.Core
import Graphics.Vty
import Graphics.Vty.Widgets.Util

data HCentered a = (Show a) => HCentered (Widget a)

instance Show (HCentered a) where
    show (HCentered _) = "HCentered { ... }"

-- |Wrap another widget to center it horizontally.
hCentered :: (Show a) => Widget a -> IO (Widget (HCentered a))
hCentered ch = do
  wRef <- newWidget (HCentered ch) $ \w ->
      w { growHorizontal_ = const $ return True

        , growVertical_ = \(HCentered child) -> growVertical child

        , render_ = \this s ctx -> do
                   HCentered child <- getState this
                   img <- render child s ctx

                   let attr' = getNormalAttr ctx
                       (half, half') = centered_halves region_width s (image_width img)

                   return $ if half > 0
                            then horiz_cat [ char_fill attr' ' ' half (image_height img)
                                           , img
                                           , char_fill attr' ' ' half' (image_height img)
                                           ]
                            else img

        , setCurrentPosition_ =
            \this pos -> do
              HCentered child <- getState this
              s <- getCurrentSize this
              chSz <- getCurrentSize child
              let (half, _) = centered_halves region_width s (region_width chSz)
                  chPos = pos `plusWidth` half
              setCurrentPosition child chPos

        , getCursorPosition_ = \this -> do
              HCentered child <- getState this
              getCursorPosition child
        }
  wRef `relayKeyEvents` ch
  wRef `relayFocusEvents` ch
  wRef `relayHideEvents` ch
  wRef `relayShowEvents` ch
  return wRef

data VCentered a = (Show a) => VCentered (Widget a)

instance Show (VCentered a) where
    show (VCentered _) = "VCentered { ... }"

-- |Wrap another widget to center it vertically.
vCentered :: (Show a) => Widget a -> IO (Widget (VCentered a))
vCentered ch = do
  wRef <- newWidget (VCentered ch) $ \w ->
      w { growVertical_ = const $ return True
        , growHorizontal_ = const $ growHorizontal ch

        , render_ = \this s ctx -> do
                   VCentered child <- getState this
                   img <- render child s ctx

                   let attr' = getNormalAttr ctx
                       (half, half') = centered_halves region_height s (image_height img)

                   return $ if half > 0
                            then vert_cat [ char_fill attr' ' ' (image_width img) half
                                          , img
                                          , char_fill attr' ' ' (image_width img) half'
                                          ]
                            else img

        , setCurrentPosition_ =
            \this pos -> do
              VCentered child <- getState this
              s <- getCurrentSize this
              chSz <- getCurrentSize child
              let (half, _) = centered_halves region_height s (region_height chSz)
                  chPos = pos `plusHeight` half
              setCurrentPosition child chPos

        , getCursorPosition_ = \this -> do
              VCentered child <- getState this
              getCursorPosition child
        }
  wRef `relayKeyEvents` ch
  wRef `relayFocusEvents` ch
  wRef `relayHideEvents` ch
  wRef `relayShowEvents` ch
  return wRef

-- |Wrap another widget to center it both vertically and horizontally.
centered :: (Show a) => Widget a -> IO (Widget (VCentered (HCentered a)))
centered wRef = vCentered =<< hCentered wRef

centered_halves :: (DisplayRegion -> Word) -> DisplayRegion -> Word -> (Word, Word)
centered_halves region_size s obj_sz =
    let remaining = region_size s - obj_sz
        half = remaining `div` 2
        half' = if remaining `mod` 2 == 0
                then half
                else half + 1
    in (half, half')
