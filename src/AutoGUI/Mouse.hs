module AutoGUI.Mouse
  ( moveTo
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

import AutoGUI
import AutoGUI.Call

-- | Move the mouse to an (x, y) position
moveTo :: Integer -> Integer -> AutoGUI ()
moveTo x y = call "moveTo" [IntArg x, IntArg y]

-- | Move the mouse to an (x, y) position, over a number of seconds
moveToDuration :: Integer -> Integer -> Double -> AutoGUI ()
moveToDuration x y duration = call "moveTo" [IntArg x, IntArg y, DoubleArg duration]

-- | Move the mouse relative to where it is now
moveRel :: Integer -> Integer -> AutoGUI ()
moveRel xOffset yOffset = call "moveRel" [IntArg xOffset, IntArg yOffset]

-- | Move the mouse relative to where it is now, over a number of seconds
moveRelDuration :: Integer -> Integer -> Double -> AutoGUI ()
moveRelDuration xOffset yOffset duration =
  call "moveRel" [IntArg xOffset, IntArg yOffset, DoubleArg duration]

-- | Click the mouse
click :: AutoGUI ()
click = call "click" []

-- | Left click the mouse
leftClick :: AutoGUI ()
leftClick = click

-- | Double click the mouse
doubleClick :: AutoGUI ()
doubleClick = call "doubleClick" []

-- | Triple click the mouse
tripleClick :: AutoGUI ()
tripleClick = call "tripleClick" []

-- | Right click the mouse
rightClick :: AutoGUI ()
rightClick = call "rightClick" []

-- | Middle click the mouse
middleClick :: AutoGUI ()
middleClick = call "middleClick" []

-- | Move the mouse to some (x, y) position and click there
moveAndClick :: Integer -> Integer -> AutoGUI ()
moveAndClick x y = moveTo x y >> click

-- | Clicks and drags the mouse through a motion of (x, y)
drag :: Integer -> Integer -> AutoGUI ()
drag x y = call "drag" [IntArg x, IntArg y]

-- | Clicks and drags the mouse through a motion of (x, y), over a number of seconds
dragDuration :: Integer -> Integer -> Double -> AutoGUI ()
dragDuration x y duration = call "drag" [IntArg x, IntArg y, DoubleArg duration]

-- | Clicks and drags the mouse to the position (x, y)
dragTo :: Integer -> Integer -> AutoGUI ()
dragTo x y = call "dragTo" [IntArg x, IntArg y]

-- | Clicks and drags the mouse to the position (x, y), over a number of seconds
dragToDuration :: Integer -> Integer -> Double -> AutoGUI ()
dragToDuration x y duration = call "dragTo" [IntArg x, IntArg y, DoubleArg duration]

-- | Clicks and drags the mouse through a motion of (x, y)
dragRel :: Integer -> Integer -> AutoGUI ()
dragRel xOffset yOffset = call "dragRel" [IntArg xOffset, IntArg yOffset]

-- | Clicks and drags the mouse through a motion of (x, y)
dragRelDuration :: Integer -> Integer -> Double -> AutoGUI ()
dragRelDuration xOffset yOffset duration =
  call "dragRel" [IntArg xOffset, IntArg yOffset, DoubleArg duration]

-- | Scroll up (positive) or down (negative)
scroll :: Double -> AutoGUI ()
scroll amount = call "scroll" [DoubleArg amount]

-- TODO: support mouseDown/mouseUp for other mouse buttons besides LMB

-- | Press the mouse button down
mouseDown :: AutoGUI ()
mouseDown = call "mouseDown" []

-- | Release the mouse button
mouseUp :: AutoGUI ()
mouseUp = call "mouseUp" []
