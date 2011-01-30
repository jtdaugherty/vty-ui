module Graphics.Vty.Widgets.Button
    ( Button
    , buttonWidget
    , newButton
    , onButtonPressed
    , pressButton
    )
where

import Data.IORef
import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util
import Graphics.Vty hiding (Button)

data Button = Button { buttonWidget :: Widget Padded
                     , buttonPressedHandlers :: IORef [Handler Button]
                     }

onButtonPressed :: (MonadIO m) => Button -> (Button -> IO ()) -> m ()
onButtonPressed = addHandler buttonPressedHandlers

pressButton :: (MonadIO m) => Button -> m ()
pressButton b = fireEvent b (return . buttonPressedHandlers) b

-- XXX set button text function

newButton :: (MonadIO m) => String -> m Button
newButton msg = do
  -- Don't hard-code... use normal attr??
  w <- simpleText msg >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  hs <- mkHandlers

  let b = Button w hs

  w `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> pressButton b
          _ -> return ()
        return False

  return b