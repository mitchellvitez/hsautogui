-- This module re-exports everything, in case you're using AutoGUI heavily and
-- don't want to deal with managing imports

module AutoGUI.All
  ( AutoGUI(..)
  , AutoGUIT(..)
  , Key
  , alert
  , click
  , confirm
  , doubleClick
  , drag
  , dragDuration
  , dragRel
  , dragRelDuration
  , dragTo
  , dragToDuration
  , failsafe
  , hotkey
  , isValidKey
  , key
  , keyDown
  , keyToText
  , keyUp
  , keys
  , leftClick
  , locateCenterOnScreen
  , locateOnScreen
  , middleClick
  , mkKey
  , mouseDown
  , mouseUp
  , moveAndClick
  , moveRel
  , moveRelDuration
  , moveTo
  , moveToDuration
  , onScreen
  , password
  , pause
  , position
  , press
  , prompt
  , rightClick
  , runAutoGUI
  , scroll
  , size
  , sleep
  , tripleClick
  , typewrite
  , typewriteKeys
  , write
  , writeWithInterval
  )
where

import AutoGUI
import AutoGUI.Debug
import AutoGUI.Info
import AutoGUI.Keyboard
import AutoGUI.Keys
import AutoGUI.MessageBoxes
import AutoGUI.Mouse
import AutoGUI.Screen
