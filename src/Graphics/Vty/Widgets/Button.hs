-- |This module provides a ''button'' widget type which has a
-- button-like appearance and generates ''press'' events.  'Button's
-- are pressed when a user presses Enter while the button has focus.
module Graphics.Vty.Widgets.Button
    ( Button
    , newButton
    , buttonWidget
    , onButtonPressed
    , pressButton
    , setButtonText
    )
where

import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util
import Graphics.Vty hiding (Button)

data Button = Button { buttonWidget :: Widget Padded
                     -- ^Get a reference to the button's widget to lay
                     -- it out.
                     , buttonText :: Widget FormattedText
                     , buttonPressedHandlers :: Handlers Button
                     }

-- |Register a handler for the button press event.
onButtonPressed :: Button -> (Button -> IO ()) -> IO ()
onButtonPressed = addHandler (return . buttonPressedHandlers)

-- |Programmatically press a button to trigger its event handlers.
pressButton :: Button -> IO ()
pressButton b = fireEvent b (return . buttonPressedHandlers) b

-- |Set the text label on a button.
setButtonText :: Button -> String -> IO ()
setButtonText b s = setText (buttonText b) s

instance HasNormalAttr Button where
    setNormalAttribute b a = setNormalAttribute (buttonWidget b) a

instance HasFocusAttr Button where
    setFocusAttribute b a = setFocusAttribute (buttonWidget b) a

-- |Create a button.  Get its underlying widget with 'buttonWidget'.
newButton :: String -> IO Button
newButton msg = do
  t <- plainText msg
  setTextAppearFocused t True

  w <- return t >>=
       withPadding (padLeftRight 3) >>=
       withNormalAttribute (white `on` black) >>=
       withFocusAttribute (blue `on` white)

  updateWidget w $ \st -> st { getCursorPosition_ = const $ return Nothing }

  hs <- newHandlers

  let b = Button w t hs

  w `onKeyPressed` \_ k _ ->
      do
        case k of
          KEnter -> pressButton b
          _ -> return ()
        return False

  return b
