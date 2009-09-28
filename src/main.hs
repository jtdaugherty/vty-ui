module Main where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM_ )

import Graphics.Vty.Widgets.Base
import Graphics.Vty.Widgets.List

import Graphics.Vty

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_green

selAttr :: Attr
selAttr = def_attr
           `with_back_color` yellow
           `with_fore_color` black

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

testHBox1 :: HBox
testHBox1 =
    let left = Text titleAttr "First"
        right = Text bodyAttr "Second"
    in HBox left right

testHBox2 :: VBox
testHBox2 =
    let left = Text titleAttr "First"
        right = Text bodyAttr "Second"
        topFill = VBox (Text titleAttr "Title") (vFill bodyAttr ' ')
    in VBox topFill (HBox left right)

testHBox3 :: VBox
testHBox3 =
    let left = Text titleAttr "- First "
        right = hFill titleAttr '-'
        topFill = VBox (Text titleAttr "Title") (vFill bodyAttr ' ')
    in VBox topFill (HBox left right)

testHBox4 :: VBox
testHBox4 =
    let top = VBox (Text titleAttr "First") (vFill bodyAttr ' ')
        bottom = HBox
                 (VBox (Text titleAttr "Left") (vFill bodyAttr ' '))
                 (VBox (Text titleAttr "Right") (vFill bodyAttr ' '))
        footer = Text titleAttr "Footer"
    in VBox (VBox top bottom) footer

listTest1 :: VBox
listTest1 = VBox
            (mkList bodyAttr selAttr 3 ["First", "Second", "Third"])
            (VBox
             (Text titleAttr "middle bar")
             (Text bodyAttr "body stuff")
            )

testWidgets :: [AnyWidget]
testWidgets = [ AnyWidget mainWidget
              , AnyWidget testVbox
              , AnyWidget testHBox1
              , AnyWidget testHBox2
              , AnyWidget testHBox3
              , AnyWidget testHBox4
              , AnyWidget listTest1
              ]

main :: IO ()
main = do
  vty <- mkVty

  forM_ testWidgets $ \w -> do
         pic_for_image <$> mkImage vty w >>= update vty
         next_event vty

  shutdown vty
