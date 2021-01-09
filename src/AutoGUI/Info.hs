module AutoGUI.Info
  ( size
  , onScreen
  , position
  )

where

import AutoGUI.Call
import AutoGUI.Discard
import Control.Monad.IO.Class
import CPython.Simple
import Data.Text (Text)
import qualified Data.Text as T

-- | (screenWidth, screenHeight) of the primary monitor in pixels
size :: IO (Integer, Integer)
size = pyautogui "size" [] []

-- | (x, y) position of the mouse
position :: IO (Integer, Integer)
position = pyautogui "position" [] []

-- | Test whether (x, y) is within the screen size
onScreen :: Integer -> Integer -> IO Bool
onScreen x y = pyautogui "onScreen" [arg x, arg y] []
