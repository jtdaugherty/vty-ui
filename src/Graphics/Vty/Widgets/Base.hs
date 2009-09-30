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
    , GrowthPolicy(..)
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
                    , string, char_fill, (<|>), (<->), image_width
                    , image_height, region_width, region_height
                    , terminal, display_bounds )

-- |The growth policy of a widget determines how its container will
-- reserve space to render it.
data GrowthPolicy = Static
                  -- ^'Static' widgets have a fixed size that is not
                  -- influenced by available space
                  | GrowVertical
                  -- ^'GrowVertical' widgets may grow vertically with
                  -- available space
                  | GrowHorizontal
                    -- ^'GrowHorizontal' widgets may grow horizontally
                    -- with available space
                    deriving (Show, Eq)

-- |The class of user interface widgets.
class Widget w where
    -- |Given a widget, render it with the given dimensions.  The
    -- resulting Image should not be larger than the specified
    -- dimensions, but may be smaller.
    render :: DisplayRegion -> w -> Image

    -- |The growth policy of this widget.
    growthPolicy :: w -> GrowthPolicy

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
-- See 'vBox'.
data VBox = forall a b. (Widget a, Widget b) => VBox a b

-- |A horizontal box layout widget capable of containing two
-- 'Widget's.  See 'hBox'.
data HBox = forall a b. (Widget a, Widget b) => HBox a b

instance Widget AnyWidget where
    growthPolicy (AnyWidget w) = growthPolicy w
    render s (AnyWidget w) = render s w

instance Widget Text where
    growthPolicy _ = Static
    render _ (Text att content) = string att content

instance Widget VFill where
    growthPolicy _ = GrowVertical
    render s (VFill att c) = char_fill att c (width s) (height s)

instance Widget HFill where
    growthPolicy _ = Static
    render s (HFill att c h) = char_fill att c (width s) (toEnum h)

instance Widget VBox where
    growthPolicy (VBox top bottom) =
        if t == GrowVertical
        then t else growthPolicy bottom
            where t = growthPolicy top

    render s (VBox top bottom) =
        t <-> b
            where
              renderHalves = let half = s `withHeight` div (height s) 2
                                 half' = if height s `mod` 2 == 0
                                         then half
                                         else region (width half) (height half + 1)
                             in ( render half top
                                , render half' bottom )
              renderTopFirst = let renderedTop = render s top
                                   renderedBottom = render s' bottom
                                   s' = s `withHeight` (height s - image_height renderedTop)
                               in (renderedTop, renderedBottom)
              renderBottomFirst = let renderedTop = render s' top
                                      renderedBottom = render s bottom
                                      s' = s `withHeight` (height s - image_height renderedBottom)
                                  in (renderedTop, renderedBottom)
              (t, b) = case (growthPolicy top, growthPolicy bottom) of
                         (GrowVertical, GrowVertical) -> renderHalves
                         (Static, _) -> renderTopFirst
                         (_, Static) -> renderBottomFirst
                         -- Horizontal contents take precedence
                         (GrowHorizontal, _) -> renderTopFirst
                         (_, GrowHorizontal) -> renderBottomFirst

instance Widget HBox where
    growthPolicy (HBox left right) =
        if l == GrowHorizontal
        then l else growthPolicy right
            where l = growthPolicy left

    render s (HBox left right) =
        t <|> b
            where
              renderHalves = let half = s `withWidth` div (width s) 2
                                 half' = if width s `mod` 2 == 0
                                         then half
                                         else region (width half + 1) (height half)
                             in ( render half left
                                , render half' right )
              renderLeftFirst = let renderedLeft = render s left
                                    renderedRight = render s' right
                                    s' = region (width s - image_width renderedLeft)
                                         (image_height renderedLeft)
                                in (renderedLeft, renderedRight)
              renderRightFirst = let renderedLeft = render s' left
                                     renderedRight = render s right
                                     s' = region (width s - image_width renderedRight)
                                          (image_height renderedRight)
                                 in (renderedLeft, renderedRight)
              (t, b) = case (growthPolicy left, growthPolicy right) of
                         (GrowHorizontal, GrowHorizontal) -> renderHalves
                         (Static, _) -> renderLeftFirst
                         (_, Static) -> renderRightFirst
                         (GrowVertical, GrowVertical) -> renderHalves
                         (_, _) -> renderLeftFirst

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
