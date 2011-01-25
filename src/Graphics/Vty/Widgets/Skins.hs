module Graphics.Vty.Widgets.Skins
    ( Skin(..)
    , asciiSkin
    , unicodeSkin
    , unicodeBoldSkin
    , unicodeRoundedSkin
    )
where

-- Corners start from top left and go clockwise.  Intersections are:
-- full, left, right, top, bottom.
data Skin = Skin { skinCornerTL :: Char
                 , skinCornerTR :: Char
                 , skinCornerBR :: Char
                 , skinCornerBL :: Char
                 , skinIntersectionFull :: Char
                 , skinIntersectionL :: Char
                 , skinIntersectionR :: Char
                 , skinIntersectionT :: Char
                 , skinIntersectionB :: Char
                 , skinHorizontal :: Char
                 , skinVertical :: Char
                 }

asciiSkin :: Skin
asciiSkin = Skin { skinCornerTL = '+'
                 , skinCornerTR = '+'
                 , skinCornerBR = '+'
                 , skinCornerBL = '+'
                 , skinIntersectionFull = '+'
                 , skinIntersectionL = '+'
                 , skinIntersectionR = '+'
                 , skinIntersectionT = '+'
                 , skinIntersectionB = '+'
                 , skinHorizontal = '-'
                 , skinVertical = '|'
                 }

unicodeSkin :: Skin
unicodeSkin = Skin { skinCornerTL = '┌'
                   , skinCornerTR = '┐'
                   , skinCornerBR = '┘'
                   , skinCornerBL = '└'
                   , skinIntersectionFull = '┼'
                   , skinIntersectionL = '├'
                   , skinIntersectionR = '┤'
                   , skinIntersectionT = '┬'
                   , skinIntersectionB = '┴'
                   , skinHorizontal = '─'
                   , skinVertical = '│'
                   }

unicodeBoldSkin :: Skin
unicodeBoldSkin = Skin { skinCornerTL = '┏'
                       , skinCornerTR = '┓'
                       , skinCornerBR = '┛'
                       , skinCornerBL = '┗'
                       , skinIntersectionFull = '╋'
                       , skinIntersectionL = '┣'
                       , skinIntersectionR = '┫'
                       , skinIntersectionT = '┳'
                       , skinIntersectionB = '┻'
                       , skinHorizontal = '━'
                       , skinVertical = '┃'
                       }

unicodeRoundedSkin :: Skin
unicodeRoundedSkin = Skin { skinCornerTL = '╭'
                          , skinCornerTR = '╮'
                          , skinCornerBR = '╯'
                          , skinCornerBL = '╰'
                          , skinIntersectionFull = '┼'
                          , skinIntersectionL = '├'
                          , skinIntersectionR = '┤'
                          , skinIntersectionT = '┬'
                          , skinIntersectionB = '┴'
                          , skinHorizontal = '─'
                          , skinVertical = '│'
                          }
