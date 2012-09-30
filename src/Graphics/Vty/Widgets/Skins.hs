-- |This module provides ''skins'' for line-drawing widgets such as
-- borders.  Different skins may be suitable for terminals with
-- different capabilities, but they are provided for greatest
-- flexibility.  Unicode skins must be used with care, as not all
-- terminals support unicode characters (but most do, these days).
module Graphics.Vty.Widgets.Skins
    ( Skin(..)
    , asciiSkin
    , unicodeSkin
    , unicodeBoldSkin
    , unicodeRoundedSkin
    )
where

import qualified Data.Text as T

-- Corners start from top left and go clockwise.  Intersections are:
-- full, left, right, top, bottom.
data Skin = Skin { skinCornerTL :: T.Text
                 , skinCornerTR :: T.Text
                 , skinCornerBR :: T.Text
                 , skinCornerBL :: T.Text
                 , skinIntersectionFull :: T.Text
                 , skinIntersectionL :: T.Text
                 , skinIntersectionR :: T.Text
                 , skinIntersectionT :: T.Text
                 , skinIntersectionB :: T.Text
                 , skinHorizontal :: T.Text
                 , skinVertical :: T.Text
                 }

-- |An ASCII skin which will work in any terminal.
asciiSkin :: Skin
asciiSkin = Skin { skinCornerTL = T.pack "+"
                 , skinCornerTR = T.pack "+"
                 , skinCornerBR = T.pack "+"
                 , skinCornerBL = T.pack "+"
                 , skinIntersectionFull = T.pack "+"
                 , skinIntersectionL = T.pack "+"
                 , skinIntersectionR = T.pack "+"
                 , skinIntersectionT = T.pack "+"
                 , skinIntersectionB = T.pack "+"
                 , skinHorizontal = T.pack "-"
                 , skinVertical = T.pack "|"
                 }

unicodeSkin :: Skin
unicodeSkin = Skin { skinCornerTL = T.pack "┌"
                   , skinCornerTR = T.pack "┐"
                   , skinCornerBR = T.pack "┘"
                   , skinCornerBL = T.pack "└"
                   , skinIntersectionFull = T.pack "┼"
                   , skinIntersectionL = T.pack "├"
                   , skinIntersectionR = T.pack "┤"
                   , skinIntersectionT = T.pack "┬"
                   , skinIntersectionB = T.pack "┴"
                   , skinHorizontal = T.pack "─"
                   , skinVertical = T.pack "│"
                   }

unicodeBoldSkin :: Skin
unicodeBoldSkin = Skin { skinCornerTL = T.pack "┏"
                       , skinCornerTR = T.pack "┓"
                       , skinCornerBR = T.pack "┛"
                       , skinCornerBL = T.pack "┗"
                       , skinIntersectionFull = T.pack "╋"
                       , skinIntersectionL = T.pack "┣"
                       , skinIntersectionR = T.pack "┫"
                       , skinIntersectionT = T.pack "┳"
                       , skinIntersectionB = T.pack "┻"
                       , skinHorizontal = T.pack "━"
                       , skinVertical = T.pack "┃"
                       }

unicodeRoundedSkin :: Skin
unicodeRoundedSkin = Skin { skinCornerTL = T.pack "╭"
                          , skinCornerTR = T.pack "╮"
                          , skinCornerBR = T.pack "╯"
                          , skinCornerBL = T.pack "╰"
                          , skinIntersectionFull = T.pack "┼"
                          , skinIntersectionL = T.pack "├"
                          , skinIntersectionR = T.pack "┤"
                          , skinIntersectionT = T.pack "┬"
                          , skinIntersectionB = T.pack "┴"
                          , skinHorizontal = T.pack "─"
                          , skinVertical = T.pack "│"
                          }
