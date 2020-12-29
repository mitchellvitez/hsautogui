module AutoGUI.Debug
  ( pause
  , failsafe
  , sleep
  )
where

import AutoGUI
import AutoGUI.Call

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float.RealFracMethods

import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Module     as Py

-- | Set a number of seconds to wait in between autogui actions
pause :: Double -> AutoGUI ()
pause seconds = setAttribute "PAUSE" $ DoubleArg seconds

-- | When set to true, move the mouse to the upper-left corner of the screen to throw
--   a Python exception, and quit the program
failsafe :: Bool -> AutoGUI ()
failsafe value = setAttribute "FAILSAFE" $ BoolArg value

-- | Sleep for a given fractional number of seconds
sleep :: Double -> IO ()
sleep n = threadDelay . fromInteger . roundDoubleInteger $ n * 1000000

setAttribute :: Text -> Arg -> AutoGUI ()
setAttribute name value = do
  autoguiModule <- ask
  liftIO $ do
    pyName <- Py.toUnicode name
    pyValue <- argToPy value
    Py.setAttribute autoguiModule pyName pyValue
