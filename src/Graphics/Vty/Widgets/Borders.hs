{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( HasBorderAttr(..)
    , Bordered
    , HBorder
    , VBorder
    , vBorder
    , hBorder
    , bordered
    , withBorderAttribute
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( Attr
    , DisplayRegion(DisplayRegion)
    , Image
    , char_fill
    , region_height
    , region_width
    , image_width
    , image_height
    , vert_cat
    , horiz_cat
    , def_attr
    )
import Graphics.Vty.Widgets.Core
    ( WidgetImpl(..)
    , Widget
    , RenderContext(..)
    , withNormalAttribute
    , newWidget
    , updateWidget
    , updateWidgetState
    , growVertical
    , growHorizontal
    , render
    , handleKeyEvent
    , getState
    , setPhysicalPosition
    )
import Graphics.Vty.Widgets.Box
    ( hBox
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Skins

class HasBorderAttr a where
    setBorderAttribute :: (MonadIO m) => a -> Attr -> m ()

data HBorder = HBorder Attr
               deriving (Show)

instance HasBorderAttr (Widget HBorder) where
    setBorderAttribute t a =
        updateWidgetState t $ \(HBorder a') -> HBorder (mergeAttr a a')

withBorderAttribute :: (MonadIO m, HasBorderAttr a) => Attr -> a -> m a
withBorderAttribute att w = setBorderAttribute w att >> return w

-- |Create a single-row horizontal border using the specified
-- attribute and character.
hBorder :: (MonadIO m) => m (Widget HBorder)
hBorder = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HBorder def_attr
        , getGrowHorizontal = const $ return True
        , draw = \this s ctx -> do
                   HBorder attr <- getState this
                   let attr' = mergeAttrs [ overrideAttr ctx
                                          , attr
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' (skinHorizontal $ skin ctx) (region_width s) 1
        }
  return wRef

data VBorder = VBorder Attr
               deriving (Show)

instance HasBorderAttr (Widget VBorder) where
    setBorderAttribute t a =
        updateWidgetState t $ \(VBorder a') -> VBorder (mergeAttr a a')

-- |Create a single-column vertical border using the specified
-- attribute and character.
vBorder :: (MonadIO m) => m (Widget VBorder)
vBorder = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VBorder def_attr
        , getGrowVertical = const $ return True
        , draw = \this s ctx -> do
                   VBorder attr <- getState this
                   let attr' = mergeAttrs [ overrideAttr ctx
                                          , attr
                                          , normalAttr ctx
                                          ]
                   return $ char_fill attr' (skinVertical $ skin ctx) 1 (region_height s)
        }
  return wRef

data Bordered a = (Show a) => Bordered Attr (Widget a)

instance Show (Bordered a) where
    show (Bordered attr _) = concat [ "Bordered { attr = "
                                    , show attr
                                    , ", ... }"
                                    ]

instance HasBorderAttr (Widget (Bordered a)) where
    setBorderAttribute t a =
        updateWidgetState t $ \(Bordered a' ch) -> Bordered (mergeAttr a a') ch

-- |Wrap a widget in a bordering box using the specified attribute.
bordered :: (MonadIO m, Show a) => Widget a -> m (Widget (Bordered a))
bordered child = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Bordered def_attr child

        , getGrowVertical = const $ growVertical child
        , getGrowHorizontal = const $ growHorizontal child

        , keyEventHandler =
            \this key mods -> do
              Bordered _ ch <- getState this
              handleKeyEvent ch key mods

        , draw =
            \this s ctx -> do
              st <- getState this
              drawBordered st s ctx

        , setPosition =
            \this pos -> do
              Bordered _ ch <- getState this
              let chPos = pos `plusWidth` 1 `plusHeight` 1
              setPhysicalPosition ch chPos
        }
  return wRef

drawBordered :: (Show a) =>
                Bordered a -> DisplayRegion -> RenderContext -> IO Image
drawBordered this s ctx = do
  let Bordered attr child = this
      attr' = mergeAttrs [ overrideAttr ctx
                         , attr
                         , normalAttr ctx
                         ]
      sk = skin ctx

  -- Render the contained widget with enough room to draw borders.
  -- Then, use the size of the rendered widget to constrain the space
  -- used by the (expanding) borders.
  let constrained = DisplayRegion (region_width s - 2) (region_height s - 2)

  childImage <- render child constrained ctx

  let adjusted = DisplayRegion (image_width childImage + 2)
                 (image_height childImage)

  tlCorner <- simpleText [skinCornerTL sk] >>= withNormalAttribute attr'
  trCorner <- simpleText [skinCornerTR sk] >>= withNormalAttribute attr'
  blCorner <- simpleText [skinCornerBL sk] >>= withNormalAttribute attr'
  brCorner <- simpleText [skinCornerBR sk] >>= withNormalAttribute attr'

  hb <- hBorder
  setBorderAttribute hb attr'

  topWidget <- hBox tlCorner =<< hBox hb trCorner
  top <- render topWidget adjusted ctx

  bottomWidget <- hBox blCorner =<< hBox hb brCorner
  bottom <- render bottomWidget adjusted ctx

  vb <- vBorder
  setBorderAttribute vb attr'

  leftRight <- render vb adjusted ctx

  let middle = horiz_cat [leftRight, childImage, leftRight]

  return $ vert_cat [top, middle, bottom]
