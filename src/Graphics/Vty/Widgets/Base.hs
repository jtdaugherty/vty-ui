{-# LANGUAGE ExistentialQuantification #-}
-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets and a type class for rendering
-- widgets to Vty 'Graphics.Vty.Image's.
--
-- Each widget type supplied by this library is exported as a type and
-- an associated constructor function (e.g., 'Text' and 'text', 'VBox'
-- and 'vBox').
module Graphics.Vty.Widgets.Base
    ( Widget(..)
    , mkImage
    , AnyWidget
    , Text
    , HBox
    , VBox
    , HFill
    , VFill
    , (<++>)
    , (<-->)
    , anyWidget
    , text
    , hBox
    , vBox
    , hFill
    , vFill
    )
where

import GHC.Word ( Word )

import Graphics.Vty ( DisplayRegion(DisplayRegion), Vty, Image, Attr
                    , string, char_fill, image_width, image_height
                    , region_width, region_height, terminal
                    , display_bounds, vert_cat, horiz_cat )

-- |The class of user interface widgets.  Note that the growth
-- properties 'growHorizontal' and 'growVertical' are used to control
-- rendering order; if a widget /can/ grow to fill available space,
-- then neighboring fixed-size widgets will be rendered first so
-- remaining space can be computed.  Then, variable-sized (growable)
-- widgets will be rendered last to consume that space.
class Widget w where
    -- |Given a widget, render it with the given dimensions.  The
    -- resulting Image should not be larger than the specified
    -- dimensions, but may be smaller.
    render :: DisplayRegion -> w -> Image

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    growHorizontal :: w -> Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    growVertical :: w -> Bool

    -- |The primary attribute of this widget, used when composing
    -- widgets.  For example, if you want to compose a widget /A/ with
    -- a space-filling widget /B/, you probably want /B/'s text
    -- attributes to be identical to those of /A/.
    primaryAttribute :: w -> Attr

    -- |Apply the specified attribute to this widget.
    withAttribute :: w -> Attr -> w

-- |A wrapper for all widget types used in normalizing heterogeneous
-- lists of widgets.  See 'anyWidget'.
data AnyWidget = forall a. (Widget a) => AnyWidget a

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
data Text = Text Attr String

-- |A vertical fill widget for filling available vertical space in a
-- box layout.  See 'vFill'.
data VFill = VFill Attr Char

-- |A horizontal fill widget for filling available horizontal space in
-- a box layout.  See 'hFill'.
data HFill = HFill Attr Char Int

-- |A vertical box layout widget capable of containing two 'Widget's.
-- See 'vBox'.  VBoxes lay out their children as follows:
--
-- * If both children are vertically expandable (e.g., both 'VBox'es),
--   the children are each given half of the parent container's
--   available space
--
-- * If one of the children is vertically expandable and the other is
--   static, the static child is rendered first and the remaining
--   space is given to the expandable child
--
-- * Otherwise, both children are rendered in top to bottom order and
--   the resulting container uses only as much space as its children
--   combined
data VBox = forall a b. (Widget a, Widget b) => VBox a b

-- |A horizontal box layout widget capable of containing two
-- 'Widget's.  See 'hBox'.  HBoxes lay out their children as follows:
--
-- * If both children are horizontally expandable (e.g., both
--   'HBox'es), the children are each given half of the parent
--   container's available space
--
-- * If one of the children is horizontally expandable and the other
--   is static, the static child is rendered first and the remaining
--   space is given to the expandable child
--
-- * Otherwise, both children are rendered in left to right order and
--   the resulting container uses only as much space as its children
--   combined
data HBox = forall a b. (Widget a, Widget b) => HBox a b

instance Widget AnyWidget where
    growHorizontal (AnyWidget w) = growHorizontal w
    growVertical (AnyWidget w) = growVertical w
    render s (AnyWidget w) = render s w
    primaryAttribute (AnyWidget w) = primaryAttribute w
    withAttribute (AnyWidget w) att = AnyWidget (withAttribute w att)

instance Widget Text where
    growHorizontal _ = False
    growVertical _ = False
    render _ (Text att content) = string att content
    primaryAttribute (Text att _) = att
    withAttribute (Text _ content) att = Text att content

instance Widget VFill where
    growHorizontal _ = False
    growVertical _ = True
    render s (VFill att c) = char_fill att c (width s) (height s)
    primaryAttribute (VFill att _) = att
    withAttribute (VFill _ c) att = VFill att c

instance Widget HFill where
    growHorizontal _ = True
    growVertical _ = False
    render s (HFill att c h) = char_fill att c (width s) (toEnum h)
    primaryAttribute (HFill att _ _) = att
    withAttribute (HFill _ c h) att = HFill att c h

instance Widget VBox where
    growHorizontal (VBox top bottom) =
        growHorizontal top || growHorizontal bottom

    growVertical (VBox top bottom) =
        growVertical top || growVertical bottom

    withAttribute (VBox top bottom) att = VBox
                                          (withAttribute top att)
                                          (withAttribute bottom att)

    -- Not the best way to choose this, but it seems like anything
    -- here is going to be arbitrary.
    primaryAttribute (VBox top _) = primaryAttribute top

    render s (VBox top bottom) =
        vert_cat ws
            where
              renderHalves = let half = s `withHeight` div (height s) 2
                                 half' = if height s `mod` 2 == 0
                                         then half
                                         else region (width half) (height half + 1)
                             in [ render half top
                                , render half' bottom ]
              renderTopFirst = let renderedTop = render s top
                                   renderedBottom = render s' bottom
                                   remaining = height s - image_height renderedTop
                                   s' = s `withHeight` remaining
                               in if image_height renderedTop >= height s
                                  then [renderedTop]
                                  else [renderedTop, renderedBottom]
              renderBottomFirst = let renderedTop = render s' top
                                      renderedBottom = render s bottom
                                      remaining = height s - image_height renderedBottom
                                      s' = s `withHeight` remaining
                                  in if image_height renderedBottom >= height s
                                     then [renderedBottom]
                                     else [renderedTop, renderedBottom]
              ws = case (growVertical top, growVertical bottom) of
                     (True, True) -> renderHalves
                     (False, _) -> renderTopFirst
                     (_, False) -> renderBottomFirst

instance Widget HBox where
    growHorizontal (HBox left right) =
        growHorizontal left || growHorizontal right

    growVertical (HBox left right) =
        growVertical left || growVertical right

    withAttribute (HBox left right) att = HBox
                                          (withAttribute left att)
                                          (withAttribute right att)

    -- Not the best way to choose this, but it seems like anything
    -- here is going to be arbitrary.
    primaryAttribute (HBox left _) = primaryAttribute left

    render s (HBox left right) =
        horiz_cat ws
            where
              renderHalves = let half = s `withWidth` div (width s) 2
                                 half' = if width s `mod` 2 == 0
                                         then half
                                         else region (width half + 1) (height half)
                             in [ render half left
                                , render half' right ]
              renderLeftFirst = let renderedLeft = render s left
                                    renderedRight = render s' right
                                    remainingWidth = width s - image_width renderedLeft
                                    s' = region remainingWidth (image_height renderedLeft)
                                in if image_width renderedLeft >= width s
                                   then [renderedLeft]
                                   else [renderedLeft, renderedRight]
              renderRightFirst = let renderedLeft = render s' left
                                     renderedRight = render s right
                                     remainingWidth = width s - image_width renderedRight
                                     s' = region remainingWidth (image_height renderedRight)
                                in if image_width renderedRight >= width s
                                   then [renderedRight]
                                   else [renderedLeft, renderedRight]
              ws = case (growHorizontal left, growHorizontal right) of
                     (True, True) -> renderHalves
                     (False, _) -> renderLeftFirst
                     (_, False) -> renderRightFirst

width :: DisplayRegion -> Word
width = region_width

height :: DisplayRegion -> Word
height = region_height

region :: Word -> Word -> DisplayRegion
region = DisplayRegion

withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h

-- |Given a 'Widget' and a 'Vty' object, render the widget using the
-- current size of the terminal controlled by Vty. Returns the
-- rendered 'Widget' as an 'Image'.
mkImage :: (Widget a) => Vty -> a -> IO Image
mkImage vty w = do
  size <- display_bounds $ terminal vty
  return $ render size w

-- |Wrap a 'Widget' in the 'AnyWidget' type for normalization
-- purposes.
anyWidget :: (Widget a) => a -> AnyWidget
anyWidget = AnyWidget

-- |Create a 'Text' widget.
text :: Attr -- ^The attribute to use to render the text
     -> String -- ^The text to display
     -> Text
text = Text

-- |Create an horizonal fill widget.
hFill :: Attr -- ^The attribute to use to render the fill
      -> Char -- ^The character to fill
      -> Int -- ^The height, in rows, of the filled area; width of the
             -- fill depends on available space
      -> HFill
hFill = HFill

-- |Create a vertical fill widget.  The dimensions of the widget will
-- depend on available space.
vFill :: Attr -- ^The attribute to use to render the fill
      -> Char -- ^The character to fill
      -> VFill
vFill = VFill

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (Widget a, Widget b) => a -- ^The left widget
     -> b -- ^The right widget
     -> HBox
hBox = HBox

-- |An alias for 'hBox' intended as sugar to chain widgets
-- horizontally.
(<++>) :: (Widget a, Widget b) => a -> b -> HBox
(<++>) = HBox

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (Widget a, Widget b) => a -> b -> VBox
vBox = VBox

-- |An alias for 'vBox' intended as sugar to chain widgets vertically.
(<-->) :: (Widget a, Widget b) => a -> b -> VBox
(<-->) = VBox
