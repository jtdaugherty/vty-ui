module Graphics.Vty.Widgets.Alignment
    ( Alignable(..)
    , Alignment(..)
    )
where

data Alignment = AlignCenter | AlignLeft | AlignRight
                 deriving (Show)

class Alignable a where
    align :: a -> Alignment -> a
