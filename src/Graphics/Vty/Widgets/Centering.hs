{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Centering
    ( HCentered
    , VCentered
    , centered
    , hCentered
    , vCentered
    )
where

import GHC.Word ( Word )
import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , getNormalAttr
    , newWidget
    , updateWidget
    , render
    , growHorizontal
    , growVertical
    , getState
    , setPhysicalPosition
    , getPhysicalSize
    )
import Graphics.Vty
    ( DisplayRegion
    , char_fill
    , region_width
    , region_height
    , image_width
    , image_height
    , vert_cat
    , horiz_cat
    )
import Graphics.Vty.Widgets.Util

data HCentered a = (Show a) => HCentered (Widget a)

instance Show (HCentered a) where
    show (HCentered _) = "HCentered { ... }"

hCentered :: (MonadIO m, Show a) => Widget a -> m (Widget (HCentered a))
hCentered ch = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HCentered ch
        , getGrowHorizontal = const $ return True

        , getGrowVertical = \(HCentered child) -> growVertical child

        , draw = \this s ctx -> do
                   HCentered child <- getState this
                   img <- render child s ctx

                   -- XXX def_attr can be wrong
                   let attr' = getNormalAttr ctx
                       (half, half') = centered_halves region_width s (image_width img)

                   return $ if half > 0
                            then horiz_cat [ char_fill attr' ' ' half (image_height img)
                                           , img
                                           , char_fill attr' ' ' half' (image_height img)
                                           ]
                            else img

        , setPosition =
            \this pos -> do
              HCentered child <- getState this
              s <- getPhysicalSize this
              chSz <- getPhysicalSize child
              let (half, _) = centered_halves region_width s (region_width chSz)
                  chPos = pos `plusWidth` half
              setPhysicalPosition child chPos
        }
  return wRef

data VCentered a = (Show a) => VCentered (Widget a)

instance Show (VCentered a) where
    show (VCentered _) = "VCentered { ... }"

vCentered :: (MonadIO m, Show a) => Widget a -> m (Widget (VCentered a))
vCentered ch = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VCentered ch
        , getGrowVertical = const $ return True
        , getGrowHorizontal = const $ growHorizontal ch

        , draw = \this s ctx -> do
                   VCentered child <- getState this
                   img <- render child s ctx

                   -- XXX def_attr can be wrong
                   let attr' = getNormalAttr ctx
                       (half, half') = centered_halves region_height s (image_height img)

                   return $ if half > 0
                            then vert_cat [ char_fill attr' ' ' (image_width img) half
                                          , img
                                          , char_fill attr' ' ' (image_width img) half'
                                          ]
                            else img

        , setPosition =
            \this pos -> do
              VCentered child <- getState this
              s <- getPhysicalSize this
              chSz <- getPhysicalSize child
              let (half, _) = centered_halves region_height s (region_height chSz)
                  chPos = pos `plusHeight` half
              setPhysicalPosition child chPos
        }
  return wRef

centered :: (MonadIO m, Show a) => Widget a -> m (Widget (VCentered (HCentered a)))
centered wRef = vCentered =<< hCentered wRef

centered_halves :: (DisplayRegion -> Word) -> DisplayRegion -> Word -> (Word, Word)
centered_halves region_size s obj_sz =
    let remaining = region_size s - obj_sz
        half = remaining `div` 2
        half' = if remaining `mod` 2 == 0
                then half
                else half + 1
    in (half, half')
