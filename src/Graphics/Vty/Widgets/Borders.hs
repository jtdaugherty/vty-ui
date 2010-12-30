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

import Control.Monad.State
    ( State
    , get
    , put
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
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , render
    )
import Graphics.Vty.Widgets.Base
    ( (<++>)
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )

data HBorder = HBorder Attr Char

-- |Create a single-row horizontal border.
hBorder :: Attr -> Widget HBorder
hBorder = hBorderWith '-'

-- |Create a single-row horizontal border using the specified
-- attribute and character.
hBorderWith :: Char -> Attr -> Widget HBorder
hBorderWith ch att =
    Widget { state = HBorder att ch
           , growVertical = False
           , growHorizontal = True
           , primaryAttribute = att
           -- XXX this is wrong, since it throws away whatever char
           -- was passed to hBorderWith.
           , withAttribute = hBorder
           , draw = \s -> return $ char_fill att ch (region_width s) 1
           }

data VBorder = VBorder Attr Char

-- |Create a single-column vertical border.
vBorder :: Attr -> Widget VBorder
vBorder = vBorderWith '|'

-- |Create a single-column vertical border using the specified
-- attribute and character.
vBorderWith :: Char -> Attr -> Widget VBorder
vBorderWith ch att =
    Widget { state = VBorder att ch
           , growHorizontal = False
           , growVertical = True
           , primaryAttribute = att
           , draw = \s -> return $ char_fill att ch 1 (region_height s)
           -- XXX wrong; see above.
           , withAttribute = vBorder
           }

data Bordered a = Bordered Attr (Widget a)

-- |Wrap a widget in a bordering box using the specified attribute.
bordered :: Attr -> Widget a -> Widget (Bordered a)
bordered att w = Widget {
                   state = Bordered att w
                 , growVertical = growVertical w
                 , growHorizontal = growHorizontal w
                 , primaryAttribute = att
                 , withAttribute = \att' -> bordered att' (withAttribute w att')
                 , draw = drawBordered
                 }

drawBordered :: DisplayRegion -> State (Bordered a) Image
drawBordered s = do
  Bordered attr child <- get

  -- Render the contained widget with enough room to draw borders.
  -- Then, use the size of the rendered widget to constrain the space
  -- used by the (expanding) borders.
  let constrained = DisplayRegion (region_width s - 2) (region_height s - 2)
      (childImage, child') = render child constrained
      adjusted = DisplayRegion (image_width childImage + 2)
                 (image_height childImage)
      corner = simpleText attr "+"
      topBottom = fst $ render (corner <++> hBorder attr <++> corner) adjusted
      leftRight = fst $ render (vBorder attr) adjusted
      middle = horiz_cat [leftRight, childImage, leftRight]

  put $ Bordered attr child'
  return $ vert_cat [topBottom, middle, topBottom]
