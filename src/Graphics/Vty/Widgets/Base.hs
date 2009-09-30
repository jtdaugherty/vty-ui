{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Base
    ( Widget(..)
    , GrowthPolicy(..)
    , AnyWidget(AnyWidget)
    , Text(Text)
    , HFill(HFill)
    , VFill(VFill)
    , VBox(VBox)
    , HBox(HBox)
    , mkImage
    )
where

import GHC.Word ( Word )

import Graphics.Vty ( DisplayRegion(DisplayRegion), Vty, Image, Attr
                    , def_attr, string, char_fill, (<|>), (<->)
                    , image_width, image_height, region_width, region_height
                    , terminal, display_bounds )

data GrowthPolicy = Static
                  | GrowVertical
                  | GrowHorizontal
                    deriving (Show, Eq)

class Widget w where
    -- Given a widget, render it with the given dimensions.  The
    -- resulting Image should not be larger than the specified
    -- dimensions, but may be smaller.
    render :: DisplayRegion -> w -> Image

    -- The attribute of the widget, if any.
    attr :: w -> Attr
    attr _ = def_attr

    growthPolicy :: w -> GrowthPolicy

data AnyWidget = forall a. (Widget a) => AnyWidget a
data Text = Text Attr String
data VFill = VFill Attr Char
data HFill = HFill Attr Char Int
data VBox = forall a b. (Widget a, Widget b) => VBox a b
data HBox = forall a b. (Widget a, Widget b) => HBox a b

instance Widget AnyWidget where
    attr (AnyWidget w) = attr w
    growthPolicy (AnyWidget w) = growthPolicy w
    render s (AnyWidget w) = render s w

instance Widget Text where
    attr (Text a _) = a
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

mkImage :: (Widget a) => Vty -> a -> IO Image
mkImage vty w = do
  size <- display_bounds $ terminal vty
  return $ render size w
