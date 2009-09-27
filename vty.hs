module Main where

import Control.Monad ( forM_ )
import Widgets

import Graphics.Vty

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_green

mainWidget :: VBox
mainWidget =
    let title = Text titleAttr " Title "
        body = Text bodyAttr "Body"
        footer = Text titleAttr " Footer"
        fill = vFill bodyAttr ' '
    in VBox title
           (VBox
            (VBox body fill)
            footer
           )

testVbox :: VBox
testVbox =
    let top = VBox (Text titleAttr "First") (vFill bodyAttr ' ')
        bottom = VBox (Text titleAttr "Third") (vFill bodyAttr ' ')
        footer = Text titleAttr "Footer"
    in VBox (VBox top bottom) footer

testWidgets :: [AnyWidget]
testWidgets = [ AnyWidget mainWidget
              , AnyWidget testVbox
              ]

main :: IO ()
main = do
  vty <- mkVty

  forM_ testWidgets $ \w -> do
         img <- mkImage vty w
         update vty $ pic_for_image img
         next_event vty

  shutdown vty
