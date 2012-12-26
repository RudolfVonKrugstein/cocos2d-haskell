module Main where

import Graphics.Cocos2d
import Graphics.Cocos2d.TouchDelegate
import Resources
import ActionManagerTest
import Data.IORef

line_SPACE = 40.0

main = cocos2dApp $ \app -> do
  mainScene <- mainMenuScene
  setAnimationInterval (1.0 / 60.0)
  runWithScene mainScene

addMainMenuMenu :: Scene -> IO ()
addMainMenuMenu scene = do
  label    <- createLabelTTF "MeinMenu" "Arial" 20
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
mainMenuScene = do
  s <- createScene
  l <- createLayerGradient (Color4b 0 0 0 255) (Color4b 0x46 0x82 0xB4 255)

  (winWidth, winHeight) <- getWinSize
  
  closeItem <- createMenuItemImage s_pathClose s_pathClose quit
  menu <- createMenuWithItems [toMenuItem closeItem]
  setPosition menu (0.0, 0.0)
  setPosition closeItem (winWidth - 30.0, winHeight -30.0)
  addChild l menu 1
  
  items <- mapM (\(t,i) -> do label <- createLabelTTF (testTitle t) "Arial" 24
                              menuItem <- createMenuItemLabel label (runTest t)
                              setPosition menuItem (winWidth /2.0, winHeight - (i + 1.0) * line_SPACE)
                              return $ toMenuItem menuItem
                ) $ zip tests [0.0..]
  
  itemMenu <- createMenuWithItems items
  addChild_ l menu

  setOnTouchesMoved s (onTouchesMoved menu)
  setOnMouseDragged s (onMouseDragged menu)
  setOnScrollWheel  s (onScrollWheel menu)
  return s

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

data Test = Test {
                  testTitle     :: String,
                  runTest       :: IO ()
                 }

actionTestScene = undefined :: IO ()
box2DTestScene = undefined :: IO ()
chipmunkTestScene = undefined :: IO ()
clickAndMoveTestScene = undefined :: IO ()
cocosDenshionTestScene = undefined :: IO ()
currentLanguageTestScene = undefined :: IO ()
drawPrimitivesTestScene = undefined :: IO ()
easeActionsTestScene = undefined :: IO ()
eventTestScene = undefined :: IO ()
extensionsTestScene = undefined :: IO ()
effectsTestScene = undefined :: IO ()
fontTestScene = undefined :: IO ()
intervalTestScene = undefined :: IO ()
labelTestScene = undefined :: IO ()
layerTestScene = undefined :: IO ()
menuTestScene = undefined :: IO ()
nodeTestScene = undefined :: IO ()
parallaxTestScene = undefined :: IO ()
particleTestScene = undefined :: IO ()
performanceTestScene = undefined :: IO ()
progressActionsTestScene = undefined :: IO ()
renderTextureTestScene = undefined :: IO ()
rotateWorldTestScene = undefined :: IO ()
sceneTestScene = undefined :: IO ()
schedulerTestScene = undefined :: IO ()
spriteTestScene = undefined :: IO ()
textInputTestScene = undefined :: IO ()
textureCacheTestScene = undefined :: IO ()
tileMapTestScene = undefined :: IO ()
touchesTestScene = undefined :: IO ()
transitionsTestScene = undefined :: IO ()
unitTestsScene = undefined :: IO ()
presentationTestsScene = undefined :: IO ()

tests :: [Test]
tests =
  [
  Test "ActionManager Test"   actionManagerTestScene,
  Test "Action Test"          actionTestScene,
  Test "Box2D Test"           box2DTestScene,
  Test "Chipmunk Test"        chipmunkTestScene,
  -- BugsTest
  Test "Click and Move Test"  clickAndMoveTestScene,
  Test "CocosDenshion Test"   cocosDenshionTestScene,
  Test "CurrentLanguage Test" currentLanguageTestScene,
  Test "DrawPrimitives Test"  drawPrimitivesTestScene,
  Test "EaseActions Test"     easeActionsTestScene,
  Test "Event Test"           eventTestScene,
  Test "Extensions Test"      extensionsTestScene,
  Test "Effects Test"         effectsTestScene,
  Test "Font Test"            fontTestScene,
  Test "Interval Test"        intervalTestScene,
  Test "Label Test"           labelTestScene,
  Test "Layer Test"           layerTestScene,
  Test "Menu Test"            menuTestScene,
  Test "Node Test"            nodeTestScene,
  Test "Parallax Test"        parallaxTestScene,
  Test "Particle Test"        particleTestScene,
  Test "Performance Test"     performanceTestScene,
  Test "ProgressActions Test" progressActionsTestScene,
  Test "RenderTexture Test"   renderTextureTestScene,
  Test "RotateWorld Test"     rotateWorldTestScene,
  Test "Scene Test"           sceneTestScene,
  Test "Scheduler Test"       schedulerTestScene,
  Test "Sprite Test"          spriteTestScene,
  Test "TextInput Test"       textInputTestScene,
  Test "TextureCache Test"    textureCacheTestScene,
  Test "TileMap Test"         tileMapTestScene,
  Test "Touches Test"         touchesTestScene,
  Test "Transitions Test"     transitionsTestScene,
  Test "Unit Tests"           unitTestsScene,
  Test "cocos2d JS Presentation" presentationTestsScene
  ]
