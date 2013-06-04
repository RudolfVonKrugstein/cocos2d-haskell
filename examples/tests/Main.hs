module Main where

import Graphics.Cocos2d
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Resources
import TestCase
import ActionManagerTest
import ActionsTest
import Data.IORef
import Haste

line_SPACE = 40.0

main = cocos2dApp $ \app -> do
  mainScene <- mainMenuScene
  setAnimationInterval (1.0 / 60.0)
  replaceScene mainScene

-- add main menu to scene
addMainMenuMenu :: Scene -> IO ()
addMainMenuMenu scene = do
  label    <- createLabelTTF "Main Menu" "Arial" 20
  menuItem <- createMenuItemLabel label goToMainMenu
  menu     <- createMenuWithItems [toMenuItem menuItem]
  setPosition menu (0.0,0.0)
  (width, _) <- getWinSize
  setPosition menuItem (width - 50.0, 25.0)
  addChild_ scene menu

-- callback to return to the main menu
goToMainMenu :: IO ()
goToMainMenu = do
  s <- mainMenuScene
  t <- createTransitionProgressRadialCCW 0.5 s
  replaceScene t

-- the main menu scene
mainMenuScene :: IO Scene
mainMenuScene = createScene $ \s -> do
  l <- createLayerGradient (Color4b 0 0 0 255) (Color4b 0x46 0x82 0xB4 255)

  (winWidth, winHeight) <- getWinSize
  
  closeItem <- createMenuItemImage s_pathClose s_pathClose quit
  setPosition closeItem (winWidth - 30.0, winHeight -30.0)
  menu <- createMenuWithItems [toMenuItem closeItem]
  setPosition menu (0.0, 0.0)
  addChild l menu 1
  
  items <- mapM (\(t,i) -> do label <- createLabelTTF (testTitle t) "Arial" 24
                              menuItem <- createMenuItemLabel label (runTestCase (testCase t) addMainMenuMenu)
                              setPosition menuItem (winWidth /2.0, winHeight - (i + 1.0) * line_SPACE)
                              return $ toMenuItem menuItem
                ) $ zip tests [0.0..]
  
  itemMenu <- createMenuWithItems items
  setPosition itemMenu (0.0, 0.0)
  addChild_ l itemMenu

  setOnTouchesMoved l (onTouchesMoved itemMenu)
  setOnMouseDragged l (onMouseDragged itemMenu)
  setOnScrollWheel  l (onScrollWheel itemMenu)
  setTouchEnabled l True

  addChild_ s l

-- callbacks

onTouchesMoved :: Menu -> [Touch] -> IO ()
onTouchesMoved menu (t:_) = do
  let (_,y) = touchDelta t
  moveMenu menu y

onMouseDragged :: Menu -> (Double,Double) -> IO ()
onMouseDragged menu (_,y) = moveMenu menu y

onScrollWheel :: Menu -> Double -> IO ()
onScrollWheel menu delta = moveMenu menu delta


-- move the menu
moveMenu :: Menu -> Double -> IO ()
moveMenu menu delta = do
  (_,y) <- getPosition menu
  let newY = min ((fromIntegral (length tests)) * line_SPACE) $ max 0.0 (y+delta)
  setPosition menu (0,newY)


emptyTestCase = TestCase emptyTestCase createScene_ emptyTestCase "Not implemented test case"
  

box2DTestCase = emptyTestCase
chipmunkTestCase = emptyTestCase
clickAndMoveTestCase = emptyTestCase
cocosDenshionTestCase = emptyTestCase
currentLanguageTestCase = emptyTestCase
drawPrimitivesTestCase = emptyTestCase
easeActionsTestCase = emptyTestCase
eventTestCase = emptyTestCase
extensionsTestCase = emptyTestCase
effectsTestCase = emptyTestCase
fontTestCase = emptyTestCase
intervalTestCase = emptyTestCase
labelTestCase = emptyTestCase
layerTestCase = emptyTestCase
menuTestCase = emptyTestCase
nodeTestCase = emptyTestCase
parallaxTestCase = emptyTestCase
particleTestCase = emptyTestCase
performanceTestCase = emptyTestCase
progressActionsTestCase = emptyTestCase
renderTextureTestCase = emptyTestCase
rotateWorldTestCase = emptyTestCase
sceneTestCase = emptyTestCase
schedulerTestCase = emptyTestCase
spriteTestCase = emptyTestCase
textInputTestCase = emptyTestCase
textureCacheTestCase = emptyTestCase
tileMapTestCase = emptyTestCase
touchesTestCase = emptyTestCase
transitionsTestCase = emptyTestCase
unitTestsCase = emptyTestCase
presentationTestCase = emptyTestCase

-- all tests
data Test = Test {
                  testTitle     :: String,
                  testCase       :: TestCase
                 }

tests :: [Test]
tests =
  [
  Test "ActionManager Test"   actionManagerTestCase,
  Test "Action Test"          actionsTestCase,
  Test "Box2D Test"           box2DTestCase,
  Test "Chipmunk Test"        chipmunkTestCase,
  -- BugsTest
  Test "Click and Move Test"  clickAndMoveTestCase,
  Test "CocosDenshion Test"   cocosDenshionTestCase,
  Test "CurrentLanguage Test" currentLanguageTestCase,
  Test "DrawPrimitives Test"  drawPrimitivesTestCase,
  Test "EaseActions Test"     easeActionsTestCase,
  Test "Event Test"           eventTestCase,
  Test "Extensions Test"      extensionsTestCase,
  Test "Effects Test"         effectsTestCase,
  Test "Font Test"            fontTestCase,
  Test "Interval Test"        intervalTestCase,
  Test "Label Test"           labelTestCase,
  Test "Layer Test"           layerTestCase,
  Test "Menu Test"            menuTestCase,
  Test "Node Test"            nodeTestCase,
  Test "Parallax Test"        parallaxTestCase,
  Test "Particle Test"        particleTestCase,
  Test "Performance Test"     performanceTestCase,
  Test "ProgressActions Test" progressActionsTestCase,
  Test "RenderTexture Test"   renderTextureTestCase,
  Test "RotateWorld Test"     rotateWorldTestCase,
  Test "Scene Test"           sceneTestCase,
  Test "Scheduler Test"       schedulerTestCase,
  Test "Sprite Test"          spriteTestCase,
  Test "TextInput Test"       textInputTestCase,
  Test "TextureCache Test"    textureCacheTestCase,
  Test "TileMap Test"         tileMapTestCase,
  Test "Touches Test"         touchesTestCase,
  Test "Transitions Test"     transitionsTestCase,
  Test "Unit Tests"           unitTestsCase,
  Test "cocos2d JS Presentation" presentationTestCase
  ]
