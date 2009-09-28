{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Base
    ( Widget(..)
    , GrowthPolicy(..)
    , AnyWidget(AnyWidget)
    , Text(Text)
    , Fill
    , hFill
    , vFill
    , VBox(VBox)
    , HBox(HBox)
    , mkImage
    )
where

import Graphics.Vty ( DisplayRegion, Vty, Image, Attr, def_attr
                    , string, char_fill, (<|>), (<->), image_width
                    , image_height, region_width, region_height
                    , terminal, display_bounds )

-- (Width, Height)
data Size = Size (Int, Int)

data GrowthPolicy = Static
                  | GrowVertical
                  | GrowHorizontal
                    deriving (Show, Eq)

class Widget w where
    -- Given a widget, render it with the given dimensions.  The
    -- resulting Image should not be larger than the specified
    -- dimensions, but may be smaller.
    render :: Size -> w -> Image

    -- The attribute of the widget, if any.
    attr :: w -> Attr
    attr _ = def_attr

    growthPolicy :: w -> GrowthPolicy

data AnyWidget = forall a. (Widget a) => AnyWidget a
data Text = Text Attr String
data Fill = Fill GrowthPolicy Attr Char
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

instance Widget Fill where
    growthPolicy (Fill gp _ _) = gp
    render (Size (w, h)) (Fill _ att c) = char_fill att c w h

instance Widget VBox where
    growthPolicy (VBox top bottom) =
        if t == GrowVertical
        then t else growthPolicy bottom
            where t = growthPolicy top

    render s (VBox top bottom) =
        t <-> b
            where
              renderHalves = let half = Size ( width s, div (height s) 2 )
                             in ( render half top
                                , render half bottom )
              renderTopFirst = let renderedTop = render s top
                                   renderedBottom = render s' bottom
                                   s' = Size ( width s
                                             , height s - (fromIntegral $ image_height renderedTop) )
                               in (renderedTop, renderedBottom)
              renderBottomFirst = let renderedTop = render s' top
                                      renderedBottom = render s bottom
                                      s' = Size ( width s
                                                , height s - (fromIntegral $ image_height renderedBottom) )
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
              renderHalves = let half = Size ( div (width s) 2, height s )
                             in ( render half left
                                , render half right )
              renderLeftFirst = let renderedLeft = render s left
                                    renderedRight = render s' right
                                    s' = Size ( width s - (fromIntegral $ image_width renderedLeft)
                                              , fromIntegral $ image_height renderedLeft )
                                in (renderedLeft, renderedRight)
              renderRightFirst = let renderedLeft = render s' left
                                     renderedRight = render s right
                                     s' = Size ( width s - (fromIntegral $ image_width renderedRight)
                                               , fromIntegral $ image_height renderedRight )
                                 in (renderedLeft, renderedRight)
              (t, b) = case (growthPolicy left, growthPolicy right) of
                         (GrowHorizontal, GrowHorizontal) -> renderHalves
                         (Static, _) -> renderLeftFirst
                         (_, Static) -> renderRightFirst
                         (GrowVertical, GrowVertical) -> renderHalves
                         (_, _) -> renderLeftFirst

width :: Size -> Int
width (Size t) = fst t

height :: Size -> Int
height (Size t) = snd t

hFill :: Attr -> Char -> Fill
hFill = Fill GrowHorizontal

vFill :: Attr -> Char -> Fill
vFill = Fill GrowVertical

regionToSize :: DisplayRegion -> Size
regionToSize db = Size ( fromIntegral $ region_width db
                       , fromIntegral $ region_height db)

mkImage :: (Widget a) => Vty -> a -> IO Image
mkImage vty w = do
  size <- display_bounds $ terminal vty
  return $ render (regionToSize size) w
