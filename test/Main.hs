module Main where

import Test.Hspec

import AutoGUI
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec $ do
  describe "basic" $ do
    it "runs AutoGUI" $ do
      runAutoGUI $
        forM_ [1..3] $ \i -> do
          write "beep"
          press [key|enter|]
          liftIO $ sleep 0.5
