module Main where

import AutoGUI.All
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  putStrLn "3 seconds until beeping begins"
  sleep 3
  runAutoGUI $
    forM_ [1..100] $ \i -> do
      write "beep"
      press [key|enter|]
      liftIO $ sleep 0.5

{- # The above is equivalent to this Python code:

import pyautogui
import time

print('3 seconds until beeping begins')
time.sleep(3)

for i in range(1, 101):
    pyautogui.write('beep')
    pyautogui.press('enter')
    time.sleep(0.5)
-}
