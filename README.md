# hsautogui

Haskell bindings for [PyAutoGUI](https://pyautogui.readthedocs.io)

## About

These are straightforward Haskell bindings for PyAutoGUI, a library for automating user interaction tasks, using haskell-cpython. 

This is just about the simplest possible example:

```haskell
import AutoGUI
main = do
  initialize
  write "Hello, world!"
```

This doesn't just print `Hello, world!` to the screen, but instead simulates a user typing it in.

## Language Comparison

The following Haskell and Python examples do the same thing:

```haskell
import AutoGUI
import Control.Monad (forM_)

main :: IO ()
main = do
  initialize
  putStrLn "3 seconds until beeping begins"
  sleep 3
  forM_ [1..100] $ \i -> do
    write "beep"
    press [key|enter|]
    sleep 0.5
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

### Debug
- `pause :: Double -> IO ()` - Set a number of seconds to wait in between autogui actions
- `failsafe :: Bool -> IO ()` - When set to true, move the mouse to the upper-left corner of the screen to throw a Python exception, and quit the program
- `sleep :: Double -> IO ()` - Sleep for a given fractional number of seconds

### Info
- `size :: IO (Integer, Integer)` - (screenWidth, screenHeight) of the primary monitor in pixels
- `position :: IO (Integer, Integer)` - (x, y) position of the mouse
- `onScreen :: Integer -> Integer -> IO Bool` - Test whether (x, y) is within the screen size

### Keyboard
- `write :: Text -> IO ()` - Write out some Text as though it were entered with the keyboard
- `typewrite :: Text -> IO ()` - Write out some Text as though it were entered with the keyboard, newline is enter
- `typewriteKeys :: [Key] -> IO ()` - Write out some Text as though it were entered with the keyboard, newline is enter
- `writeWithInterval :: Text -> Double -> IO ()` - Write out some Text as though it were entered with the keyboard, with a specified number of seconds between keypresses
- `press :: Key -> IO ()` - Simulate a keypress
- `keyDown :: Key -> IO ()` - Simulate holding a key down
- `keyUp :: Key -> IO ()` - Simulate releasing a key
- `hotkey :: [Key] -> IO ()` - Press a key combination

### Keys
- `key :: QuasiQuoter` - This quasiquoter lets you use [key|enter|] at compile time, so you don't get a Maybe as you would from mkKey
- `mkKey :: Text -> Maybe Key`
- `keyToText :: Key -> Text`
- `isValidKey :: Text -> Bool`
- `keys :: Set Key`

### Message Boxes
- `alert :: Text -> IO ()` - Show a box onscreen until dismissed
- `confirm :: Text -> IO Bool` - Show a box onscreen until a user hits OK or Cancel. Return True on OK, False on Cancel, and False if user closes the box
- `password :: Text -> IO Text` - Show a box onscreen, allowing user to enter some screened text. Return empty string if user closes the box
- `prompt :: Text -> IO Text` - Show a box onscreen, allowing user to enter some text. Return empty string if user closes the box

### Mouse
- `moveTo :: Integer -> Integer -> IO ()` - Move the mouse to an (x, y) position
- `moveToDuration :: Integer -> Integer -> Double -> IO ()` - Move the mouse to an (x, y) position, over a number of seconds
- `moveRel :: Integer -> Integer -> IO ()` - Move the mouse relative to where it is now
- `moveRelDuration :: Integer -> Integer -> Double -> IO ()` - Move the mouse relative to where it is now, over a number of seconds
- `click :: MouseButton -> IO ()` - Click the specified mouse button
- `leftClick :: IO ()` - Left click the mouse
- `doubleClick :: IO ()` - Double click the mouse
- `tripleClick :: IO ()` - Triple click the mouse
- `rightClick :: IO ()` - Right click the mouse
- `middleClick :: IO ()` - Middle click the mouse
- `moveAndClick :: Integer -> Integer -> IO ()` - Move the mouse to some (x, y) position and click there
- `drag :: Integer -> Integer -> IO ()` - Clicks and drags the mouse through a motion of (x, y)
- `dragDuration :: Integer -> Integer -> Double -> IO ()` - Clicks and drags the mouse through a motion of (x, y), over a number of seconds
- `dragTo :: Integer -> Integer -> IO ()` - Clicks and drags the mouse to the position (x, y)
- `dragToDuration :: Integer -> Integer -> Double -> IO ()` - Clicks and drags the mouse to the position (x, y), over a number of seconds
- `dragRel :: Integer -> Integer -> IO ()` - Clicks and drags the mouse through a motion of (x, y)
- `dragRelDuration :: Integer -> Integer -> Double -> IO ()` - Clicks and drags the mouse through a motion of (x, y)
- `scroll :: Integer -> IO ()` - Scroll up (positive) or down (negative)
- `mouseDown :: IO ()` - Press the mouse button down
- `mouseUp :: IO ()` - Release the mouse button

### Screen
- `locateOnScreen :: FilePath -> IO (Maybe (Integer, Integer, Integer, Integer))` - Return (left, top, width, height) of first place the image is found
- `locateCenterOnScreen :: FilePath -> IO (Maybe (Integer, Integer))` - Return (x, y) of center of an image, if the image is found

