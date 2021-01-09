module AutoGUI.Call
  ( pyautogui )
where

import CPython.Simple
import CPython.Simple.Instances
import Data.Text (Text)

pyautogui :: FromPy a => Text -> [Arg] -> [(Text, Arg)] -> IO a
pyautogui = call "pyautogui"
