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

import AutoGUI
import AutoGUI.Call
import AutoGUI.Keys

import Data.Text (Text)
import qualified Data.Text as T

-- | Write out some Text as though it were entered with the keyboard
write :: Text -> AutoGUI ()
write msg = call "write" [TextArg msg]

-- | Write out some Text as though it were entered with the keyboard, newline is enter
typewrite :: Text -> AutoGUI ()
typewrite msg = call "typewrite" [TextArg msg]

-- | Write out some Text as though it were entered with the keyboard, newline is enter
typewriteKeys :: [Key] -> AutoGUI ()
typewriteKeys keys = call "typewrite" $ KeyArg <$> keys

-- | Write out some Text as though it were entered with the keyboard, with a specified
--   number of seconds between keypresses
writeWithInterval :: Text -> Double -> AutoGUI ()
writeWithInterval msg interval = call "write" [TextArg msg, DoubleArg interval]

-- | Simulate a keypress
press :: Key -> AutoGUI ()
press key = call "press" [KeyArg key]

-- | Simulate holding a key down
keyDown :: Key -> AutoGUI ()
keyDown key = call "keyDown" [KeyArg key]

-- | Simulate releasing a key
keyUp :: Key -> AutoGUI ()
keyUp key = call "keyUp" [KeyArg key]

-- | Press a key combination
hotkey :: [Key] -> AutoGUI ()
hotkey keys = call "hotkey" $ KeyArg <$> keys
