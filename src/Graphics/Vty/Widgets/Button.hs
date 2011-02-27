module Graphics.Vty.Widgets.Button
    ( Button
    , buttonWidget
    , newButton
    , onButtonPressed
    , pressButton
    , setButtonText
    )
where

import Control.Monad.Trans
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util
import Graphics.Vty hiding (Button)

data Button = Button { buttonWidget :: Widget Padded
                     , buttonText :: Widget FormattedText
                     , buttonPressedHandlers :: Handlers Button
                     }

onButtonPressed :: (MonadIO m) => Button -> (Button -> IO ()) -> m ()
onButtonPressed = addHandler (return . buttonPressedHandlers)

pressButton :: (MonadIO m) => Button -> m ()
pressButton b = fireEvent b (return . buttonPressedHandlers) b

setButtonText :: (MonadIO m) => Button -> String -> m ()
setButtonText b s = setText (buttonText b) s

instance HasNormalAttr Button where
    setNormalAttribute b a = setNormalAttribute (buttonWidget b) a

instance HasFocusAttr Button where
    setFocusAttribute b a = setFocusAttribute (buttonWidget b) a

newButton :: (MonadIO m) => String -> m Button
newButton msg = do
  t <- plainText msg

  w <- return t >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  hs <- newHandlers

  let b = Button w t hs

  w `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> pressButton b
          _ -> return ()
        return False

  return b