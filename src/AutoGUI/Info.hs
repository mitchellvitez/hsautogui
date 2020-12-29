module AutoGUI.Info
  ( size
  , onScreen
  , position
  )

where

import AutoGUI
import AutoGUI.Call

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Module     as Py

-- | (screenWidth, screenHeight) of the primary monitor in pixels
size :: AutoGUI (Integer, Integer)
size = getXY "size"

-- | (x, y) position of the mouse
position :: AutoGUI (Integer, Integer)
position = getXY "position"

-- | Test whether (x, y) is within the screen size
onScreen :: Integer -> Integer -> AutoGUI Bool
onScreen x y = do
  pyObjBool <- call' "onScreen" [IntArg x, IntArg y]
  liftIO $ Py.toBool pyObjBool

getXY :: Text -> AutoGUI (Integer, Integer)
getXY func = do
  pyObjTuple <- call' func []
  liftIO $ do
    Just tuple <- Py.cast pyObjTuple
    [pyObjWidth, pyObjHeight] <- Py.fromTuple tuple
    Just pyWidth <- Py.cast pyObjWidth
    Just pyHeight <- Py.cast pyObjHeight
    width <- Py.fromInteger pyWidth
    height <- Py.fromInteger pyHeight
    pure (width, height)
