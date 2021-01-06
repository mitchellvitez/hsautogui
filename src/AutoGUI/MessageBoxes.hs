module AutoGUI.MessageBoxes
  ( alert
  , confirm
  , password
  , prompt
  )
where

import AutoGUI.Call
import CPython.Simple
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

-- | Show a box onscreen until dismissed
alert :: Text -> IO ()
alert msg = pyautogui "alert" [arg msg] []

-- | Show a box onscreen until a user hits OK or Cancel
--   Return True on OK, False on Cancel, and False if user closes the box
confirm :: Text -> IO Bool
confirm msg = do
  okOrCancel :: Text <- pyautogui "confirm" [arg msg] []
  pure $ okOrCancel == "OK"

-- | Show a box onscreen, allowing user to enter some screened text
--   Return empty string if user closes the box
password :: Text -> IO Text
password msg = textInput "password" msg

-- | Show a box onscreen, allowing user to enter some text
--   Return empty string if user closes the box
prompt :: Text -> IO Text
prompt msg = textInput "prompt" msg

textInput :: Text -> Text -> IO Text
textInput func msg = do
  pyPrompt :: Maybe Text <- pyautogui func [arg msg] []
  pure $ case pyPrompt of
    Just promptText -> promptText
    Nothing -> ""
