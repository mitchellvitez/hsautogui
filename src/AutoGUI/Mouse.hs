{-# LANGUAGE TypeApplications #-}

module AutoGUI.Mouse
  ( MouseButton(..)
  , moveTo
  , moveToDuration
  , moveRel
  , moveRelDuration
  , click
  , leftClick
  , doubleClick
  , tripleClick
  , rightClick
  , middleClick
  , moveAndClick
  , drag
  , dragDuration
  , dragTo
  , dragToDuration
  , dragRel
  , dragRelDuration
  , scroll
  , mouseDown
  , mouseUp
  )
where

import AutoGUI.Call
import CPython.Simple
import CPython.Simple.Instances
import Data.Text

data MouseButton
  = LeftMouseButton
  | RightMouseButton
  | MiddleMouseButton

-- TODO: try to make this as easy/automatic as possible
instance ToPy MouseButton where
  toPy LeftMouseButton   = toPy @Text "left"
  toPy RightMouseButton  = toPy @Text "right"
  toPy MiddleMouseButton = toPy @Text "middle"

-- | Move the mouse to an (x, y) position
moveTo :: Integer -> Integer -> IO ()
moveTo x y = pyautogui "moveTo" [arg x, arg y] []

-- | Move the mouse to an (x, y) position, over a number of seconds
moveToDuration :: Integer -> Integer -> Double -> IO ()
moveToDuration x y duration =
  pyautogui "moveTo" [arg x, arg y] [("duration", arg duration)]

-- | Move the mouse relative to where it is now
moveRel :: Integer -> Integer -> IO ()
moveRel xOffset yOffset =
  pyautogui "moveRel" [arg xOffset, arg yOffset] []

-- | Move the mouse relative to where it is now, over a number of seconds
moveRelDuration :: Integer -> Integer -> Double -> IO ()
moveRelDuration xOffset yOffset duration =
  pyautogui "moveRel" [arg xOffset, arg yOffset, arg duration] []

-- | Click a specified mouse button
click :: MouseButton -> IO ()
click button = pyautogui "click" [] [("button", arg button)]

-- | Double click the mouse
doubleClick :: IO ()
doubleClick = pyautogui "doubleClick" [] []

-- | Triple click the mouse
tripleClick :: IO ()
tripleClick = pyautogui "tripleClick" [] []

-- | Left click the mouse
leftClick :: IO ()
leftClick = click LeftMouseButton

-- | Right click the mouse
rightClick :: IO ()
rightClick = click RightMouseButton

-- | Middle click the mouse
middleClick :: IO ()
middleClick = click MiddleMouseButton

-- | Move the mouse to some (x, y) position and click there
moveAndClick :: Integer -> Integer -> IO ()
moveAndClick x y = moveTo x y >> click LeftMouseButton

-- | Clicks and drags the mouse through a motion of (x, y)
drag :: Integer -> Integer -> IO ()
drag x y = pyautogui "drag" [arg x, arg y] []

-- | Clicks and drags the mouse through a motion of (x, y), over a number of seconds
dragDuration :: Integer -> Integer -> Double -> IO ()
dragDuration x y duration = pyautogui "drag" [arg x, arg y, arg duration] []

-- | Clicks and drags the mouse to the position (x, y)
dragTo :: Integer -> Integer -> IO ()
dragTo x y = pyautogui "dragTo" [arg x, arg y] []

-- | Clicks and drags the mouse to the position (x, y), over a number of seconds
dragToDuration :: Integer -> Integer -> Double -> IO ()
dragToDuration x y duration = pyautogui "dragTo" [arg x, arg y, arg duration] []

-- | Clicks and drags the mouse through a motion of (x, y)
dragRel :: Integer -> Integer -> IO ()
dragRel xOffset yOffset = pyautogui "dragRel" [arg xOffset, arg yOffset] []

-- | Clicks and drags the mouse through a motion of (x, y)
dragRelDuration :: Integer -> Integer -> Double -> IO ()
dragRelDuration xOffset yOffset duration =
  pyautogui "dragRel" [arg xOffset, arg yOffset, arg duration] []

-- | Scroll up (positive) or down (negative)
scroll :: Integer -> IO ()
scroll amount = pyautogui "scroll" [arg amount] []

-- | Press the left mouse button down
mouseDown :: IO ()
mouseDown = pyautogui "mouseDown" [] []

-- | Release the left mouse button
mouseUp :: IO ()
mouseUp = pyautogui "mouseUp" [] []

-- | Press a specified mouse button
mouseButtonDown :: MouseButton -> IO ()
mouseButtonDown button = pyautogui "mouseDown" [] [("button", arg button)]

-- | Press a specified mouse button
mouseButtonUp :: MouseButton -> IO ()
mouseButtonUp button = pyautogui "mouseUp" [] [("button", arg button)]
