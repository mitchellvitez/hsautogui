module AutoGUI.Debug
  ( pause
  , failsafe
  , sleep
  )
where

import AutoGUI.Discard

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import CPython.Simple
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float.RealFracMethods

-- | Set a number of seconds to wait in between autogui actions
pause :: Double -> IO ()
pause = setAttribute "pyautogui" "PAUSE"

-- | When set to true, move the mouse to the upper-left corner of the screen to throw
--   a Python exception, and quit the program
failsafe :: Bool -> IO ()
failsafe = setAttribute "pyautogui" "FAILSAFE"

-- | Sleep for a given fractional number of seconds
sleep :: Double -> IO ()
sleep n = threadDelay . fromInteger . roundDoubleInteger $ n * 1000000
