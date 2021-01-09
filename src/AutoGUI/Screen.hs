module AutoGUI.Screen
  ( locateOnScreen
  , locateCenterOnScreen
  , Color(..)
  )
where

import AutoGUI.Call
import AutoGUI.Discard
import CPython.Simple
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

type Color = (Integer, Integer, Integer)

-- TODO: Returns a Pillow/PIL Image object. Not supported yet.
-- screenshot :: AutoGUI Py.SomeObject
-- screenshot = call' "screenshot" []

-- | Return (left, top, width, height) of first place the image is found
locateOnScreen :: FilePath -> IO (Maybe (Integer, Integer, Integer, Integer))
locateOnScreen path =
  pyautogui "locateOnScreen" [arg $ T.pack path] []

-- TODO: Returns a Python generator. Convert that to a Haskell list. Not supported yet.
-- locateAllOnScreen :: FilePath -> IO [(Integer, Integer, Integer, Integer)]
-- locateAllOnScreen path = pyautogui "locateAllOnScreen" [arg $ T.pack path] []

-- | Return (x, y) of center of an image, if the image is found
locateCenterOnScreen :: FilePath -> IO (Maybe (Integer, Integer))
locateCenterOnScreen path =
  pyautogui "locateCenterOnScreen" [arg $ T.pack path] []

pixelMatchesColor :: Integer -> Integer -> Color -> IO Bool
pixelMatchesColor x y color =
  pyautogui "pixelMatchesColor" [arg x, arg y, arg color] []

pixelMatchesColorWithTolerance :: Integer -> Integer -> Color -> Double -> IO Bool
pixelMatchesColorWithTolerance x y color tolerance =
  pyautogui "pixelMatchesColor" [arg x, arg y, arg color] [("tolerance", arg tolerance)]
