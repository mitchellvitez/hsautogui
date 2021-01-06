module AutoGUI.Discard where

import CPython.Simple.Instances
import qualified CPython.Types.Tuple as Py (fromTuple, toTuple)
import qualified CPython.Types.Unicode as Py
import Data.Typeable
import qualified Data.Text as T

-- TODO: Cpython.Simple should export the stuff from CPython.Simple.Instances

-- TODO toPy Bool
instance ToPy Bool where
  toPy b = error "nope"

-- TODO fromPy Bool
instance FromPy Bool where
  fromPy _ = pure False

-- TODO FilePath
-- instance {-# OVERLAPS #-} ToPy String where
  -- toPy = easyToPy Py.toUnicode . T.pack

-- TODO: fromPy (a, b, c, d)
instance (FromPy a, FromPy b, FromPy c, FromPy d) => FromPy (a, b, c, d) where
  fromPy val = do
    [pyA, pyB, pyC, pyD] <- easyFromPy Py.fromTuple Proxy val
    a <- fromPy pyA
    b <- fromPy pyB
    c <- fromPy pyC
    d <- fromPy pyD
    pure (a, b, c, d)

instance (ToPy a, ToPy b, ToPy c) => ToPy (a, b, c) where
  toPy (a, b, c) = do
    pyA <- toPy a
    pyB <- toPy b
    pyC <- toPy c
    easyToPy Py.toTuple [pyA, pyB, pyC]
