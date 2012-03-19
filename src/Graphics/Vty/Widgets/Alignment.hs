-- |This module provides a type and a type class for expressing
-- alignment.  For concrete uses, see the Table and ProgressBar
-- modules.
module Graphics.Vty.Widgets.Alignment
    ( Alignment(..)
    , Alignable(..)
    )
where

-- |Column alignment values.
data Alignment = AlignCenter | AlignLeft | AlignRight
                 deriving (Show)

-- |The class of types whose values or contents can be aligned.
class Alignable a where
    align :: a -> Alignment -> a
