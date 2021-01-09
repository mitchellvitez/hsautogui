module AutoGUI.Keyboard
  ( write
  , typewrite
  , typewriteKeys
  , writeWithInterval
  , press
  , keyDown
  , keyUp
  , hotkey
  )
where

import AutoGUI.Keys
import AutoGUI.Call

import CPython.Simple
import CPython.Simple.Instances
import Data.Text (Text)
import qualified Data.Text as T

-- | Write out some Text as though it were entered with the keyboard
write :: Text -> IO ()
write msg = pyautogui "write" [arg msg] []

-- | Write out some Text as though it were entered with the keyboard, newline is enter
typewrite :: Text -> IO ()
typewrite msg = pyautogui "typewrite" [arg msg] []

-- | Write out some Text as though it were entered with the keyboard, newline is enter
typewriteKeys :: [Key] -> IO ()
typewriteKeys keys = pyautogui "typewrite" (map arg keys) []

-- | Write out some Text as though it were entered with the keyboard, with a specified
--   number of seconds between keypresses
writeWithInterval :: Text -> Double -> IO ()
writeWithInterval msg interval = pyautogui "write" [arg msg, arg interval] []

-- | Simulate a keypress
press :: Key -> IO ()
press key = pyautogui "press" [arg key] []

-- | Simulate holding a key down
keyDown :: Key -> IO ()
keyDown key = pyautogui "keyDown" [arg key] []

-- | Simulate releasing a key
keyUp :: Key -> IO ()
keyUp key = pyautogui "keyUp" [arg key] []

-- | Press a key combination
hotkey :: [Key] -> IO ()
hotkey keys = pyautogui "hotkey" (map arg keys) []
