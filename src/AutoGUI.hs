module AutoGUI
  ( runAutoGUI
  , AutoGUIT(..)
  , AutoGUI(..)
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Module     as Py

newtype AutoGUIT m a =
  AutoGUIT { unAutoGUI :: ReaderT Py.Module m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Py.Module)

type AutoGUI = AutoGUIT IO

runAutoGUI :: AutoGUI a -> IO a
runAutoGUI autogui = do
  Py.initialize
  autoguiModule <- Py.importModule "pyautogui"
  runReaderT (unAutoGUI autogui) autoguiModule
