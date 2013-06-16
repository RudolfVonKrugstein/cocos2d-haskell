module Main where

import Graphics.Cocos2d
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Resources
import TestCase
import Tests
import MainMenu
import Data.IORef

import Haste


main = cocos2dApp $ \app -> do
  mainScene <- mainMenuScene
  setAnimationInterval (1.0 / 60.0)
  replaceScene mainScene

