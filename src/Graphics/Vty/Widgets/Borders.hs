-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( Bordered
    , HBorder
    , VBorder
    , vBorder
    , hBorder
    , vBorderWith
    , hBorderWith
    , bordered
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
    )
import Graphics.Vty.Widgets.Core
    ( WidgetImpl(..)
    , Widget
    , newWidget
    , updateWidget
    , growVertical
    , growHorizontal
    , render
    , handleKeyEvent
    , getState
    , withWidth
    , withHeight
    , setPhysicalPosition
    )
import Graphics.Vty.Widgets.Box
    ( hBox
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )

data HBorder = HBorder Attr Char

-- |Create a single-row horizontal border.
hBorder :: (MonadIO m) => Attr -> m (Widget HBorder)
hBorder = hBorderWith '-'

-- |Create a single-row horizontal border using the specified
-- attribute and character.
hBorderWith :: (MonadIO m) => Char -> Attr -> m (Widget HBorder)
hBorderWith ch att = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HBorder att ch
        , getGrowVertical = return False
        , getGrowHorizontal = return True
        , draw = \this s _ _ mAttr -> do
                   HBorder attr _ <- getState this
                   let attr' = maybe attr id mAttr
                   return $ char_fill attr' ch (region_width s) 1
        }
  return wRef

data VBorder = VBorder Attr Char

-- |Create a single-column vertical border.
vBorder :: (MonadIO m) => Attr -> m (Widget VBorder)
vBorder = vBorderWith '|'

-- |Create a single-column vertical border using the specified
-- attribute and character.
vBorderWith :: (MonadIO m) => Char -> Attr -> m (Widget VBorder)
vBorderWith ch att = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VBorder att ch
        , getGrowHorizontal = return False
        , getGrowVertical = return True
        , draw = \this s _ _ mAttr -> do
                   VBorder attr _ <- getState this
                   let attr' = maybe attr id mAttr
                   return $ char_fill attr' ch 1 (region_height s)
        }
  return wRef

data Bordered a = Bordered Attr (Widget a)

-- |Wrap a widget in a bordering box using the specified attribute.
bordered :: (MonadIO m) => Attr -> Widget a -> m (Widget (Bordered a))
bordered att child = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Bordered att child

        , getGrowVertical = growVertical child
        , getGrowHorizontal = growHorizontal child

        , keyEventHandler =
            \this key mods -> do
              Bordered _ ch <- getState this
              handleKeyEvent ch key mods

        , draw =
            \this s normAttr focAttr mAttr -> do
              st <- getState this
              drawBordered st s normAttr focAttr mAttr

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              Bordered _ ch <- getState this
              let chPos = pos
                          `withWidth` (region_width pos + 1)
                          `withHeight` (region_height pos + 1)
              setPhysicalPosition ch chPos
        }
  return wRef

drawBordered :: Bordered a -> DisplayRegion -> Attr -> Attr -> Maybe Attr -> IO Image
drawBordered this s normAttr focAttr mAttr = do
  let Bordered attr child = this
      attr' = maybe attr id mAttr

  -- Render the contained widget with enough room to draw borders.
  -- Then, use the size of the rendered widget to constrain the space
  -- used by the (expanding) borders.
  let constrained = DisplayRegion (region_width s - 2) (region_height s - 2)

  childImage <- render child constrained normAttr focAttr mAttr

  let adjusted = DisplayRegion (image_width childImage + 2)
                 (image_height childImage)
  corner <- simpleText attr' "+"

  hb <- hBorder attr'
  topWidget <- hBox corner =<< hBox hb corner
  topBottom <- render topWidget adjusted normAttr focAttr mAttr

  vb <- vBorder attr'
  leftRight <- render vb adjusted normAttr focAttr mAttr

  let middle = horiz_cat [leftRight, childImage, leftRight]

  return $ vert_cat [topBottom, middle, topBottom]
