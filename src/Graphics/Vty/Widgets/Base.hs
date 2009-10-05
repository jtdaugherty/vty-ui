{-# LANGUAGE ExistentialQuantification #-}
-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets and a type class for rendering
-- widgets to Vty 'Graphics.Vty.Image's.
--
-- Each widget type supplied by this library is exported as a type and
-- an associated constructor function (e.g., 'Text' and 'text', 'Box'
-- and 'vBox' / 'hBox').
module Graphics.Vty.Widgets.Base
    ( Widget(..)
    , mkImage
    , AnyWidget
    , Text
    , Box
    , Fill
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

-- |A fill widget for filling available vertical or horizontal space
-- in a box layout.  See 'vFill' and 'hFill'.
data Fill = VFill Attr Char
          | HFill Attr Char Int

data Orientation = Horizontal | Vertical

-- |A box layout widget capable of containing two 'Widget's
-- horizontally or vertically.  See 'hBox' and 'vBox'.  Boxes lay out
-- their children as follows:
--
-- * If both children are expandable in the same dimension (i.e., both
--   vertically or both horizontally), the children are each given
--   half of the parent container's available space
--
-- * If one of the children is expandable and the other is static, the
--   static child is rendered first and the remaining space is given
--   to the expandable child
--
-- * Otherwise, both children are rendered in top-to-bottom or
--   left-to-right order and the resulting container uses only as much
--   space as its children combined
data Box = forall a b. (Widget a, Widget b) => Box Orientation a b

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

instance Widget Fill where
    growHorizontal (HFill _ _ _) = True
    growHorizontal (VFill _ _) = False

    growVertical (VFill _ _) = True
    growVertical (HFill _ _ _) = False

    primaryAttribute (HFill att _ _) = att
    primaryAttribute (VFill att _) = att

    withAttribute (HFill _ c h) att = HFill att c h
    withAttribute (VFill _ c) att = VFill att c

    render s (VFill att c) = char_fill att c (width s) (height s)
    render s (HFill att c h) = char_fill att c (width s) (toEnum h)

instance Widget Box where
    growHorizontal (Box _ a b) =
        growHorizontal a || growHorizontal b

    growVertical (Box _ a b) =
        growVertical a || growVertical b

    withAttribute (Box o top bottom) att = Box o
                                           (withAttribute top att)
                                           (withAttribute bottom att)

    -- Not the best way to choose this, but it seems like anything
    -- here is going to be arbitrary.
    primaryAttribute (Box _ top _) = primaryAttribute top

    render s (Box Vertical top bottom) =
        renderBox s (top, bottom) growVertical vert_cat region_height image_height withHeight
    render s (Box Horizontal left right) =
        renderBox s (left, right) growHorizontal horiz_cat region_width image_width withWidth

renderBox :: (Widget a, Widget b) =>
             DisplayRegion
          -> (a, b)
          -> (AnyWidget -> Bool) -- growth comparison function
          -> ([Image] -> Image) -- concatenation function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Image -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> Image
renderBox s (first, second) grow concatenate regDimension imgDimension withDim =
    concatenate ws
        where
          ws = case (grow $ anyWidget first, grow $ anyWidget second) of
                 (True, True) -> renderHalves
                 (False, _) -> renderOrdered first second
                 (_, False) -> let [a, b] = renderOrdered second first
                               in [b, a]
          renderHalves = let half = s `withDim` div (regDimension s) 2
                             half' = if regDimension s `mod` 2 == 0
                                     then half
                                     else half `withDim` (regDimension half + 1)
                         in [ render half first
                            , render half' second ]
          renderOrdered a b = let renderedA = render s a
                                  renderedB = render s' b
                                  remaining = regDimension s - imgDimension renderedA
                                  s' = s `withDim` remaining
                              in if imgDimension renderedA >= regDimension s
                                 then [renderedA]
                                 else [renderedA, renderedB]

width :: DisplayRegion -> Word
width = region_width

height :: DisplayRegion -> Word
height = region_height

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
      -> Fill
hFill = HFill

-- |Create a vertical fill widget.  The dimensions of the widget will
-- depend on available space.
vFill :: Attr -- ^The attribute to use to render the fill
      -> Char -- ^The character to fill
      -> Fill
vFill = VFill

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (Widget a, Widget b) => a -- ^The left widget
     -> b -- ^The right widget
     -> Box
hBox = Box Horizontal

-- |An alias for 'hBox' intended as sugar to chain widgets
-- horizontally.
(<++>) :: (Widget a, Widget b) => a -> b -> Box
(<++>) = hBox

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (Widget a, Widget b) => a -- ^The top widget
     -> b -- ^The bottom widget
     -> Box
vBox = Box Vertical

-- |An alias for 'vBox' intended as sugar to chain widgets vertically.
(<-->) :: (Widget a, Widget b) => a -> b -> Box
(<-->) = vBox
