module Main where

-- This demo is discussed in the vty-ui user's manual.

import Control.Monad
import Control.Monad.Trans

import Graphics.Vty
import Graphics.Vty.Widgets.All

data PhoneNumber = PhoneNumber String String String
                   deriving (Show)

-- This type isn't pretty, but we have to specify the type of the
-- complete interface.  Initially you can let the compiler tell you
-- what it is.
type T = Box (Box
              (Box (Box (HFixed Edit) FormattedText) (HFixed Edit))
              FormattedText) (HFixed Edit)

data PhoneInput =
   PhoneInput { phoneInputWidget :: Widget T
              , edit1 :: Widget Edit
              , edit2 :: Widget Edit
              , edit3 :: Widget Edit
              , activateHandlers :: Handlers PhoneNumber
              }

newPhoneInput :: (MonadIO m) => m (PhoneInput, Widget FocusGroup)
newPhoneInput = do
   ahs <- newHandlers
   e1 <- editWidget
   e2 <- editWidget
   e3 <- editWidget
   ui <- (hFixed 4 e1) <++>
         (plainText "-") <++>
         (hFixed 4 e2) <++>
         (plainText "-") <++>
         (hFixed 5 e3)

   setEditMaxLength e1 3
   setEditMaxLength e2 3
   setEditMaxLength e3 4

   let w = PhoneInput ui e1 e2 e3 ahs
       doFireEvent = const $ do
         num <- mkPhoneNumber
         fireEvent w (return . activateHandlers) num

       mkPhoneNumber = do
         s1 <- getEditText e1
         s2 <- getEditText e2
         s3 <- getEditText e3
         return $ PhoneNumber s1 s2 s3

   e1 `onActivate` doFireEvent
   e2 `onActivate` doFireEvent
   e3 `onActivate` doFireEvent

   e1 `onChange` \s -> when (length s == 3) $ focus e2
   e2 `onChange` \s -> when (length s == 3) $ focus e3

   fg <- newFocusGroup
   mapM_ (addToFocusGroup fg) [e1, e2, e3]
   return (w, fg)

onPhoneInputActivate :: (MonadIO m) => PhoneInput
                     -> (PhoneNumber -> IO ()) -> m ()
onPhoneInputActivate input handler =
    addHandler (return . activateHandlers) input handler

main :: IO ()
main = do
  (p, fg) <- newPhoneInput
  p `onPhoneInputActivate` (error . show)

  ui <- padded (phoneInputWidget p) (padLeftRight 5 `pad` padTopBottom 2)

  c <- newCollection
  _ <- addToCollection c ui fg

  runUi c $ defaultContext { focusAttr = white `on` blue
                           }
