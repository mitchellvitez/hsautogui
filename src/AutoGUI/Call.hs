module AutoGUI.Call where

import AutoGUI.Run
import AutoGUI.Keys

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T

import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Module     as Py

data Arg
  = IntArg Integer
  | TextArg Text
  | KeyArg Key
  | DoubleArg Double
  | BoolArg Bool

argToPy :: Arg -> IO Py.SomeObject
argToPy = \case
  IntArg    i -> Py.toObject <$> Py.toInteger i
  TextArg   t -> Py.toObject <$> Py.toUnicode t
  KeyArg    k -> Py.toObject <$> Py.toUnicode (keyToText k)
  DoubleArg d -> Py.toObject <$> Py.toFloat d
  -- there isn't a bool equivalent to the above in haskell-cpython, just convert to int
  BoolArg   b -> Py.toObject <$> Py.toInteger (if b then 1 else 0)

call' :: Text -> [Arg] -> AutoGUI Py.SomeObject
call' func args = do
  autoguiModule <- ask
  liftIO $ do
    pyFunc <- Py.getAttribute autoguiModule =<< Py.toUnicode func
    pyArgs <- mapM argToPy args
    Py.callArgs pyFunc pyArgs

call :: Text -> [Arg] -> AutoGUI ()
call func args = call' func args >> pure ()
