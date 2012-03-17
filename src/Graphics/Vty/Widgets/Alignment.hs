module Graphics.Vty.Widgets.Alignment
    ( Alignment(..)
    , Alignable(..)
    )
where

-- |Column alignment values.
data Alignment = AlignCenter | AlignLeft | AlignRight
                 deriving (Show)

-- |The class of types whose values can be aligned.
class Alignable a where
    align :: a -> Alignment -> a
