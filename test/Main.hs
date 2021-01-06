module Main where

import Test.Hspec

import AutoGUI
import CPython.Simple
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec $ do
  describe "basic" $ do
    it "runs" $ do
      initialize
      pause 1.0
