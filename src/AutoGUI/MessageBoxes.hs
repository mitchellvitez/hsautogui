module AutoGUI.MessageBoxes
  ( alert
  , confirm
  , password
  , prompt
  )
where

import AutoGUI
import AutoGUI.Call

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Module     as Py

-- | Show a box onscreen until dismissed
alert :: Text -> AutoGUI ()
alert msg = call "alert" [TextArg msg]

-- | Show a box onscreen until a user hits OK or Cancel
--   Return True on OK, False on Cancel, and False if user closes the box
confirm :: Text -> AutoGUI Bool
confirm msg = do
  pyObjOkOrCancel <- call' "confirm" [TextArg msg]
  liftIO $ do
    pyOkOrCancelCasted <- Py.cast pyObjOkOrCancel
    case pyOkOrCancelCasted of
      Nothing -> pure False
      Just pyOkOrCancel -> do
        okOrCancel <- Py.fromUnicode pyOkOrCancel
        pure $ if okOrCancel == "OK" then True else False

-- | Show a box onscreen, allowing user to enter some screened text
--   Return empty string if user closes the box
password :: Text -> AutoGUI Text
password msg = textInput "password" msg

-- | Show a box onscreen, allowing user to enter some text
--   Return empty string if user closes the box
prompt :: Text -> AutoGUI Text
prompt msg = textInput "prompt" msg

textInput :: Text -> Text -> AutoGUI Text
textInput func msg = do
  pyObjPrompt <- call' func [TextArg msg]
  liftIO $ do
    pyPrompt <- Py.cast pyObjPrompt
    case pyPrompt of
      Just promptText -> Py.fromUnicode promptText
      Nothing -> pure ""
