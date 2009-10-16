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
    ( AnyWidget
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

import Graphics.Vty.Widgets.Rendering
import Graphics.Vty ( DisplayRegion, Attr, string, char_fill
                    , region_width, region_height )

-- |A wrapper for all widget types used in normalizing heterogeneous
-- lists of widgets.  See 'anyWidget'.
data AnyWidget = forall a. (Widget a) => AnyWidget a

-- |Wrap a 'Widget' in the 'AnyWidget' type for normalization
-- purposes.
anyWidget :: (Widget a) => a -> AnyWidget
anyWidget = AnyWidget

instance Widget AnyWidget where
    growHorizontal (AnyWidget w) = growHorizontal w
    growVertical (AnyWidget w) = growVertical w
    render s (AnyWidget w) = render s w
    primaryAttribute (AnyWidget w) = primaryAttribute w
    withAttribute (AnyWidget w) att = AnyWidget (withAttribute w att)

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
data Text = Text Attr String

instance Widget Text where
    growHorizontal _ = False
    growVertical _ = False
    render _ (Text att content) = renderImg $ string att content
    primaryAttribute (Text att _) = att
    withAttribute (Text _ content) att = Text att content

-- |A fill widget for filling available vertical or horizontal space
-- in a box layout.  See 'vFill' and 'hFill'.
data Fill = VFill Attr Char
          | HFill Attr Char Int

instance Widget Fill where
    growHorizontal (HFill _ _ _) = True
    growHorizontal (VFill _ _) = False

    growVertical (VFill _ _) = True
    growVertical (HFill _ _ _) = False

    primaryAttribute (HFill att _ _) = att
    primaryAttribute (VFill att _) = att

    withAttribute (HFill _ c h) att = HFill att c h
    withAttribute (VFill _ c) att = VFill att c

    render s (VFill att c) = renderImg $ char_fill att c (region_width s) (region_height s)
    render s (HFill att c h) = renderImg $ char_fill att c (region_width s) (toEnum h)

-- |A box layout widget capable of containing two 'Widget's
-- horizontally or vertically.  See 'hBox' and 'vBox'.  Boxes lay out
-- their children by using the growth properties of the children:
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
        renderBox s (top, bottom) Vertical growVertical region_height renderHeight withHeight
    render s (Box Horizontal left right) =
        renderBox s (left, right) Horizontal growHorizontal region_width renderWidth withWidth

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: (Widget a, Widget b) =>
             DisplayRegion
          -> (a, b)
          -> Orientation
          -> (AnyWidget -> Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Render -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> Render
renderBox s (first, second) orientation grow regDimension renderDimension withDim =
    renderMany orientation ws
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
                                  remaining = regDimension s - renderDimension renderedA
                                  s' = s `withDim` remaining
                              in if renderDimension renderedA >= regDimension s
                                 then [renderedA]
                                 else [renderedA, renderedB]

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
