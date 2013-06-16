module Tests (
              Test(..),
              tests
)
where

import TestCase
import Graphics.Cocos2d
import Graphics.Cocos2d.Scene
import ActionManagerTest
import ActionsTest

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
  Test "ActionManager Test"   (actionManagerTestCase),
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
