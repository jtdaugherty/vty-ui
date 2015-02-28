module Graphics.Vty.Widgets.Scrollable
    ( ScrollVertically(..)
    )
where

class ScrollVertically a where
    scrollVerticallyBy :: a -> Int -> IO ()
    pageUp :: a -> IO ()
    pageDown :: a -> IO ()
    scrollToBeginning :: a -> IO ()
    scrollToEnd :: a -> IO ()

    scrollUp :: a -> IO ()
    scrollUp w = scrollVerticallyBy w (-1)
    scrollDown :: a -> IO ()
    scrollDown w = scrollVerticallyBy w 1
