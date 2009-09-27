{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Graphics.Vty

-- (Width, Height)
data Size = Size (Int, Int)

width :: Size -> Int
width (Size t) = fst t

height :: Size -> Int
height (Size t) = snd t

data SizingPolicy = StaticSize | DependentSize
                    deriving (Show, Eq)

class Widget w where
    -- Given a widget, render it with the given dimensions.  The
    -- resulting Image should not be larger than the specified
    -- dimensions, but may be smaller.
    render :: Size -> w -> Image

    -- The attribute of the widget, if any.
    attr :: w -> Attr
    attr _ = def_attr

    sizingPolicy :: w -> SizingPolicy

data Wrapper = forall a. (Widget a) => Wrapper a
data TextWidget = TextWidget Attr String
data FillWidget = FillWidget Attr Char
data VBoxWidget = forall a b. (Widget a, Widget b) => VBoxWidget a b
data HBoxWidget = forall a b. (Widget a, Widget b) => HBoxWidget a b

instance Widget Wrapper where
    attr (Wrapper w) = attr w
    sizingPolicy (Wrapper w) = sizingPolicy w
    render s (Wrapper w) = render s w

instance Widget TextWidget where
    attr (TextWidget a _) = a
    sizingPolicy _ = StaticSize
    render _ (TextWidget attr content) = string attr content

instance Widget FillWidget where
    sizingPolicy _ = DependentSize
    render (Size (w, h)) (FillWidget attr c) = char_fill attr c w h

instance Widget VBoxWidget where
    sizingPolicy (VBoxWidget top bottom) =
        if sizingPolicy top == DependentSize
        then DependentSize
        else sizingPolicy bottom

    -- vertical box: if the sizing policy of both children is
    -- dependent, give them equal space.  Otherwise, render the static
    -- one first and give the rest of the space to the dynamic widget.
    -- Kinda ugly but we can clean it up later.
    render s@(Size (w, h)) (VBoxWidget top bottom) =
        uncurry (<->) rendered
            where
              rendered = case (sizingPolicy top, sizingPolicy bottom) of
                           (DependentSize, DependentSize) ->
                               ( render half top
                               , render half bottom )
                               where
                                 half = Size ( width s
                                             , div (height s) 2 )
                           (StaticSize, DependentSize) ->
                               (renderedTop, renderedBottom)
                               where
                                 renderedTop = render s top
                                 renderedBottom = render s' bottom
                                 s' = Size ( width s
                                           , height s - (fromIntegral $ image_height renderedTop) )
                           (DependentSize, StaticSize) ->
                               (renderedTop, renderedBottom)
                               where
                                 renderedTop = render s' top
                                 renderedBottom = render s bottom
                                 s' = Size ( width s
                                           , height s - (fromIntegral $ image_height renderedBottom) )

instance Widget HBoxWidget where
    sizingPolicy (HBoxWidget left right) =
        if sizingPolicy left == DependentSize
        then DependentSize
         else sizingPolicy right

    render s@(Size (w, h)) (HBoxWidget left right) =
        uncurry (<|>) rendered
            where
              rendered = case (sizingPolicy left, sizingPolicy right) of
                           (DependentSize, DependentSize) ->
                               ( render half left
                               , render half right )
                               where
                                 half = Size ( div (width s) 2
                                             , height s )
                           (StaticSize, DependentSize) ->
                               (renderedLeft, renderedRight)
                               where
                                 renderedLeft = render s left
                                 renderedRight = render s' right
                                 s' = Size ( width s - (fromIntegral $ image_width renderedLeft)
                                           , fromIntegral $ image_height renderedLeft )
                           (DependentSize, StaticSize) ->
                               (renderedLeft, renderedRight)
                               where
                                 renderedLeft = render s' left
                                 renderedRight = render s right
                                 s' = Size ( width s - (fromIntegral $ image_width renderedRight)
                                           , fromIntegral $ image_height renderedRight )

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_green

regionToSize :: DisplayRegion -> Size
regionToSize db = Size ( fromIntegral $ region_width db
                       , fromIntegral $ region_height db)

vboxLayout :: (Widget a) => Size -> [a] -> VBoxWidget
vboxLayout children = undefined

mkImage :: (Widget a) => Vty -> a -> IO Image
mkImage vty w = do
  size <- display_bounds $ terminal vty
  return $ render (regionToSize size) w

mainWidget :: VBoxWidget
mainWidget =
    let title = HBoxWidget
                (TextWidget titleAttr " Title ")
                (FillWidget titleAttr '-')
        body = TextWidget bodyAttr "Body"
        footer = TextWidget titleAttr " Footer"
        fill = FillWidget bodyAttr ' '
    in VBoxWidget title
           (VBoxWidget
            (VBoxWidget body fill)
            footer)

main :: IO ()
main = do
  vty <- mkVty
  img <- mkImage vty mainWidget
  update vty $ pic_for_image img
  event <- next_event vty
  shutdown vty
