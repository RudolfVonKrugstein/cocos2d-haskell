module ActionManagerTest where

import Cocos2d
import Resources

-- some constants
tag_GROSSINI = 5561
tag_SEQUENCE = 5562

data ActionManagerTest = AMT { prev :: ActionManagerTest,
                               this :: IO Scene,
                               next :: ActionManagerTest,
                               title :: String}

-- Generic creation of test scene
startSceneFromActionManagerTest :: ActionManagerTest -> IO ()
startSceneFromActionManagerTest amt = do
  (winWidth,winHeight) <- getWinSize
  scene <- (this amt)
  
  label <- createLabelTTF (title amt) "Arial" 32
  addChild scene label 1
  setPosition label (winWidth/2.0, winHeight -50.0)
  
  item1 <- createMenuItemImage s_pathB1 s_pathB2 (startSceneFromActionManagerTest (prev amt))
  item2 <- createMenuItemImage s_pathR1 s_pathR2 (startSceneFromActionManagerTest amt)
  item3 <- createMenuItemImage s_pathF1 s_pathF2 (startSceneFromActionManagerTest (next amt))

  menu <- createMenuWithItems [item1,item2,item3]

  setPosition menu (0.0,0.0)
  (width2,height2) <- getContentSize item2

  setPosition item1 (winWidth /2.0 - width2 *2.0, height2 /2.0)
  setPosition item2 (winWidth/2.0, height2 /2.0)
  setPosition item3 (winWidth/2.0 + width2 * 2.0, height2 /2.0)
  
  addChild scene menu 1

  replaceScene scene

------------------------------------------------------------------
-- Test1
------------------------------------------------------------------
crashTest :: ActionManagerTest
crashTest = AMT resumeTest crashTestScene logicTest "Test 1. Should not crash"

crashTestScene :: IO Scene
crashTestScene = do
  scene <- createScene

  child <- createSprite s_pathGrossini
  setPosition child (200.0,200.0)
  addChild scene child 1
 
  -- Sum of all actions's duration is 1.5 second 
  runAction child (RotateBy 1.5 90.0)
  runAction child (Sequence [DelayTime 0.4, FadeOut 1.1])
  
  -- after 1.4 seconds, scene will be removed
  runAction scene (Sequence [DelayTime 1.4, CallFunc $ removeScene scene])
  return scene

removeScene :: Scene -> IO ()
removeScene scene = do
  p <- getParent scene
  removeChild p scene
  startSceneFromActionManagerTest (next crashTest)

------------------------------------------------------------------
-- Test2
------------------------------------------------------------------
logicTest :: ActionManagerTest
logicTest = AMT crashTest logicTestScene pauseTest "Logic Test"

logicTestScene :: IO Scene
logicTestScene = do
  scene <- createScene

  grossini <- createSprite s_pathGrossini  
  
  setTag grossini 2
  addChild scene grossini 0

  setPosition grossini (200.0, 200.0)

  runAction grossini (Sequence [MoveBy 1.0 (150.0,0.0), CallFunc $ onBugMe grossini])
  return scene

onBugMe :: Sprite -> IO ()
onBugMe sprite = do
  stopAllActions sprite
  runAction sprite (ScaleTo 2.0 (2.0,2.0))

------------------------------------------------------------------
-- PauseTest
------------------------------------------------------------------
pauseTest :: ActionManagerTest
pauseTest = AMT logicTest pauseTestScene removeTest "Pause Test"

pauseTestScene :: IO Scene
pauseTestScene = do
  scene <- createScene
  -- This test MUST be done in 'onEnter' and not on 'init'
  -- otherwise the paused action will be resumed at 'onEnter' time
  -- so ... this probably does not work :(
  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF "After 5 seconds grossini should move" "Thonburi" 16
  addChild_ scene l
  setPosition l (winWidth /2.0, 245.0)
  -- Also, this test MUST be done, after [super onEnter]
  -- Also probably problematic in haskell
  grossini <- createSprite s_pathGrossini
  setTag grossini tag_GROSSINI
  addChild scene grossini 0

  scheduleOnce scene (onUnpause scene) 3

  return scene

onUnpause :: Scene -> IO ()
onUnpause scene = do
  node <- getChildByTag scene tag_GROSSINI
  resumeTarget node

------------------------------------------------------------------
-- RemoveTest
------------------------------------------------------------------
removeTest :: ActionManagerTest
removeTest = AMT pauseTest removeTestScene resumeTest "Remove Test"

removeTestScene :: IO Scene
removeTestScene = do
  scene <- createScene

  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF "Should not crash" "Thonburi" 16

  addChild_ scene l
  setPosition l (winWidth / 2.0, 245.0)

  child <- createSprite s_pathGrossini
  setPosition child (200.0, 200.0)
  setTag child tag_GROSSINI

  addChild scene scene 1
  runAction child (TagAction tag_SEQUENCE $ Sequence [MoveBy 2.0 (200.0,0.0), CallFunc $ stopAction scene])

  return scene

stopAction :: Scene -> IO ()
stopAction scene = do
  sprite <- getChildByTag scene tag_GROSSINI
  stopActionByTag sprite tag_SEQUENCE

------------------------------------------------------------------
-- ResumeTest
------------------------------------------------------------------
resumeTest :: ActionManagerTest
resumeTest = AMT removeTest resumeTestScene crashTest "Resume Test"

resumeTestScene :: IO Scene
resumeTestScene = do
  scene <- createScene

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

  return scene

resumeGrossini :: Scene -> IO ()
resumeGrossini scene = do
  grossini <- getChildByTag scene tag_GROSSINI
  resumeTarget grossini

