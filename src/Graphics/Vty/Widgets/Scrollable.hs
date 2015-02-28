module Graphics.Vty.Widgets.Scrollable
    ( Scrollable(..)
    )
where

class Scrollable a where
    scrollBy :: a -> Int -> IO ()
    pageUp :: a -> IO ()
    pageDown :: a -> IO ()
    scrollToBeginning :: a -> IO ()
    scrollToEnd :: a -> IO ()

    scrollUp :: a -> IO ()
    scrollUp w = scrollBy w (-1)
    scrollDown :: a -> IO ()
    scrollDown w = scrollBy w 1
