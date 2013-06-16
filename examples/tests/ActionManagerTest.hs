module ActionManagerTest where

import Graphics.Cocos2d
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Resources
import Haste (alert)
import TestCase
import {-# SOURCE #-} MainMenu

actionManagerTestCase = crashTest

-- some constants
tag_GROSSINI = 5561
tag_SEQUENCE = 5562

------------------------------------------------------------------
-- Test1
------------------------------------------------------------------
crashTest = TestCase resumeTest crashTestScene logicTest "Test 1. Should not crash"

crashTestScene :: IO Scene
crashTestScene = createScene $ \scene -> do
  layer <- createLayer
  addChild_ scene layer

  child <- createSprite s_pathGrossini
  setPosition child (200.0,200.0)
  addChild layer child 1
 
  -- Sum of all actions's duration is 1.5 second 
  runAction child (RotateBy 1.5 90.0)
  runAction child (Sequence [DelayTime 1.4, FadeOut 1.1])
  
  -- after 1.4 seconds, scene will be removed
  runAction layer (Sequence [DelayTime 1.4, CallFunc $ removeLayer layer])

removeLayer :: Layer -> IO ()
removeLayer layer = do
  p <- getParent layer
  removeChild p layer
  runTestCase (next crashTest)

------------------------------------------------------------------
-- Test2
------------------------------------------------------------------
logicTest = TestCase crashTest logicTestScene pauseTest "Logic Test"

logicTestScene :: IO Scene
logicTestScene = createScene $ \scene -> do
  grossini <- createSprite s_pathGrossini  
  
  setTag grossini 2
  addChild scene grossini 0

  setPosition grossini (200.0, 200.0)

  runAction grossini (Sequence [MoveBy 1.0 (150.0,0.0), CallFunc $ onBugMe grossini])

onBugMe :: Sprite -> IO ()
onBugMe sprite = do
  stopAllActions sprite
  runAction sprite (ScaleTo 2.0 (2.0,2.0))

------------------------------------------------------------------
-- PauseTest
------------------------------------------------------------------
pauseTest = TestCase logicTest pauseTestScene removeTest "Pause Test"

pauseTestScene :: IO Scene
pauseTestScene = createScene $ \scene -> do
  -- This test MUST be done in 'onEnter' and not on 'init'
  -- otherwise the paused action will be resumed at 'onEnter' time
  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF "After 5 seconds grossini should move" "Thonburi" 16
  addChild_ scene l
  setPosition l (winWidth /2.0, 245.0)
  -- Also, this test MUST be done, after [super onEnter]
  grossini <- createSprite s_pathGrossini
  setTag grossini tag_GROSSINI
  addChild scene grossini 0
  setPosition grossini (200.0, 200.0)
  addAction (MoveBy 1.0 (150.0, 0.0)) grossini True

  scheduleOnce scene (onUnpause scene) 3.0

onUnpause :: Scene -> IO ()
onUnpause scene = do
  node <- getChildByTag scene tag_GROSSINI
  resumeTarget node

------------------------------------------------------------------
-- RemoveTest
------------------------------------------------------------------
removeTest = TestCase pauseTest removeTestScene resumeTest "Remove Test"

removeTestScene :: IO Scene
removeTestScene = createScene $ \scene -> do
  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF "Should not crash" "Thonburi" 16

  addChild_ scene l
  setPosition l (winWidth / 2.0, 245.0)

  child <- createSprite s_pathGrossini
  setPosition child (200.0, 200.0)
  setTag child tag_GROSSINI

  addChild scene child 1
  runAction child (TagAction tag_SEQUENCE $ Sequence [MoveBy 2.0 (200.0,0.0), CallFunc $ stopAction scene])

stopAction :: Scene -> IO ()
stopAction scene = do
  sprite <- getChildByTag scene tag_GROSSINI
  stopActionByTag sprite tag_SEQUENCE

------------------------------------------------------------------
-- ResumeTest
------------------------------------------------------------------
resumeTest = TestCase removeTest resumeTestScene crashTest "Resume Test"

resumeTestScene :: IO Scene
resumeTestScene = createScene $ \scene -> do
  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF "Grossini only rotate/scale in 3 seconds" "Thonburi" 16
  addChild_ scene l
  setPosition l (winWidth/2.0, 245.0)

  grossini <- createSprite s_pathGrossini
  setTag grossini tag_GROSSINI

  addChild scene grossini 0
  setPosition grossini (winWidth/2.0, winHeight/2.0)

  runAction grossini (ScaleBy 2.0 (2.0,2.0))
  pauseTarget grossini

  runAction grossini (RotateBy 2.0 360.0)  

  scheduleOnce scene (resumeGrossini scene) 3.0

resumeGrossini :: Scene -> IO ()
resumeGrossini scene = do
  grossini <- getChildByTag scene tag_GROSSINI
  resumeTarget grossini

