module AutoGUI.Screen
  ( locateOnScreen
  , locateCenterOnScreen
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

-- TODO: Returns a Pillow/PIL Image object. Not nicely supported yet.
-- screenshot :: AutoGUI Py.SomeObject
-- screenshot = call' "screenshot" []

-- | Return (left, top, width, height) of first place the image is found
locateOnScreen :: FilePath -> AutoGUI (Maybe (Integer, Integer, Integer, Integer))
locateOnScreen path = do
  pyObjTuple <- call' "locateOnScreen" [TextArg $ T.pack path]
  isNone <- liftIO $ Py.isNone pyObjTuple
  if isNone
    then pure Nothing
    else liftIO $ do
      Just tuple <- Py.cast pyObjTuple
      [pyObjLeft, pyObjTop, pyObjWidth, pyObjHeight] <- Py.fromTuple tuple
      Just pyLeft <- Py.cast pyObjLeft
      Just pyTop <- Py.cast pyObjTop
      Just pyWidth <- Py.cast pyObjWidth
      Just pyHeight <- Py.cast pyObjHeight
      left <- Py.fromInteger pyLeft
      top <- Py.fromInteger pyTop
      width <- Py.fromInteger pyWidth
      height <- Py.fromInteger pyHeight
      pure $ Just (left, top, width, height)

-- TODO: Returns a Python generator. Convert that to a Haskell list.
-- locateAllOnScreen :: FilePath -> AutoGUI [(Integer, Integer, Integer, Integer)]
-- locateAllOnScreen path = call' "locateAllOnScreen" [TextArg $ T.pack path]

-- | Return (x, y) of center of an image, if the image is found
locateCenterOnScreen :: FilePath -> AutoGUI (Maybe (Integer, Integer))
locateCenterOnScreen path = do
  pyObjTuple <- call' "locateCenterOnScreen" [TextArg $ T.pack path]
  isNone <- liftIO $ Py.isNone pyObjTuple
  if isNone
    then pure Nothing
    else liftIO $ do
      Just tuple <- Py.cast pyObjTuple
      [pyObjWidth, pyObjHeight] <- Py.fromTuple tuple
      Just pyWidth <- Py.cast pyObjWidth
      Just pyHeight <- Py.cast pyObjHeight
      width <- Py.fromInteger pyWidth
      height <- Py.fromInteger pyHeight
      pure $ Just (width, height)

-- TODO pixelMatchesColor
