# hsautogui

Haskell bindings for [PyAutoGUI](https://pyautogui.readthedocs.io)

## About

These are straightforward Haskell bindings for PyAutoGUI, a library for automating user interaction tasks, using haskell-cpython. 

This is just about the simplest possible example:

```haskell
import AutoGUI
main = runAutoGUI $ write "Hello, world!"
```

This doesn't just print `Hello, world!` to the screen, but instead simulates a user typing it in.

## Language Comparison

When using this library, the following Haskell and Python examples do the same thing:

```haskell
import AutoGUI
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
```

```python
import pyautogui
import time

print('3 seconds until beeping begins')
time.sleep(3)

for i in range(1, 101):
    pyautogui.write('beep')
    pyautogui.press('enter')
    time.sleep(0.5)
```

## Constructing a `Key`

Because not all valid `Text`s are valid `Key`s, we need a way to check that `Key`s are valid when creating them. This leads to `mkKey :: Text -> Maybe Key`. However, using the `key` quasiquoter, we can sidestep having to use `Maybe` by catching invalid keys at compile time. For example, `[key|backspace|]` is a valid `Key` which we can construct and check at compile time.

This is especially useful for some data that looks like this, where there are way too many values (and values with strange characters) for a sum type to be especially handy, but we want to check validity in some way. We generally know which keys we want to use at compile time.

## Overview

### General

- `runAutoGUI :: AutoGUI a -> IO a` runs `AutoGUI` actions in `IO`

### Debug
- `pause :: Double -> AutoGUI ()` - Set a number of seconds to wait in between autogui actions
- `failsafe :: Bool -> AutoGUI ()` - When set to true, move the mouse to the upper-left corner of the screen to throw a Python exception, and quit the program
- `sleep :: Double -> IO ()` - Sleep for a given fractional number of seconds

### Info
- `size :: AutoGUI (Integer, Integer)` - (screenWidth, screenHeight) of the primary monitor in pixels
- `position :: AutoGUI (Integer, Integer)` - (x, y) position of the mouse
- `onScreen :: Integer -> Integer -> AutoGUI Bool` - Test whether (x, y) is within the screen size

### Keyboard
- `write :: Text -> AutoGUI ()` - Write out some Text as though it were entered with the keyboard
- `typewrite :: Text -> AutoGUI ()` - Write out some Text as though it were entered with the keyboard, newline is enter
- `typewriteKeys :: [Key] -> AutoGUI ()` - Write out some Text as though it were entered with the keyboard, newline is enter
- `writeWithInterval :: Text -> Double -> AutoGUI ()` - Write out some Text as though it were entered with the keyboard, with a specified number of seconds between keypresses
- `press :: Key -> AutoGUI ()` - Simulate a keypress
- `keyDown :: Key -> AutoGUI ()` - Simulate holding a key down
- `keyUp :: Key -> AutoGUI ()` - Simulate releasing a key
- `hotkey :: [Key] -> AutoGUI ()` - Press a key combination

### Keys
- `key :: QuasiQuoter` - This quasiquoter lets you use [key|enter|] at compile time, so you don't get a Maybe as you would from mkKey
- `mkKey :: Text -> Maybe Key`
- `keyToText :: Key -> Text`
- `isValidKey :: Text -> Bool`
- `keys :: Set Key`

### MessageBoxes
- `alert :: Text -> AutoGUI ()` - Show a box onscreen until dismissed
- `confirm :: Text -> AutoGUI Bool` - Show a box onscreen until a user hits OK or Cancel. Return True on OK, False on Cancel, and False if user closes the box
- `password :: Text -> AutoGUI Text` - Show a box onscreen, allowing user to enter some screened text. Return empty string if user closes the box
- `prompt :: Text -> AutoGUI Text` - Show a box onscreen, allowing user to enter some text. Return empty string if user closes the box

### Mouse
- `moveTo :: Integer -> Integer -> AutoGUI ()` - Move the mouse to an (x, y) position
- `moveToDuration :: Integer -> Integer -> Double -> AutoGUI ()` - Move the mouse to an (x, y) position, over a number of seconds
- `moveRel :: Integer -> Integer -> AutoGUI ()` - Move the mouse relative to where it is now
- `moveRelDuration :: Integer -> Integer -> Double -> AutoGUI ()` - Move the mouse relative to where it is now, over a number of seconds
- `click :: AutoGUI ()` - Click the mouse
- `leftClick :: AutoGUI ()` - Left click the mouse
- `doubleClick :: AutoGUI ()` - Double click the mouse
- `tripleClick :: AutoGUI ()` - Triple click the mouse
- `rightClick :: AutoGUI ()` - Right click the mouse
- `middleClick :: AutoGUI ()` - Middle click the mouse
- `moveAndClick :: Integer -> Integer -> AutoGUI ()` - Move the mouse to some (x, y) position and click there
- `drag :: Integer -> Integer -> AutoGUI ()` - Clicks and drags the mouse through a motion of (x, y)
- `dragDuration :: Integer -> Integer -> Double -> AutoGUI ()` - Clicks and drags the mouse through a motion of (x, y), over a number of seconds
- `dragTo :: Integer -> Integer -> AutoGUI ()` - Clicks and drags the mouse to the position (x, y)
- `dragToDuration :: Integer -> Integer -> Double -> AutoGUI ()` - Clicks and drags the mouse to the position (x, y), over a number of seconds
- `dragRel :: Integer -> Integer -> AutoGUI ()` - Clicks and drags the mouse through a motion of (x, y)
- `dragRelDuration :: Integer -> Integer -> Double -> AutoGUI ()` - Clicks and drags the mouse through a motion of (x, y)
- `scroll :: Integer -> AutoGUI ()` - Scroll up (positive) or down (negative)
- `mouseDown :: AutoGUI ()` - Press the mouse button down
- `mouseUp :: AutoGUI ()` - Release the mouse button

### Screen
- `locateOnScreen :: FilePath -> AutoGUI (Maybe (Integer, Integer, Integer, Integer))` - Return (left, top, width, height) of first place the image is found
- `locateCenterOnScreen :: FilePath -> AutoGUI (Maybe (Integer, Integer))` - Return (x, y) of center of an image, if the image is found

