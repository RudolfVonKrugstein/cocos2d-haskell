module Main where

addMainMenuMenu :: Scene -> IO ()
addMainMenuMenu s = do
  label    <- createLabelTTF "MeinMenu" "Arial" 20
  menuItem <- createMenuItemLabel label goToMainMenu
  menu     <- createMenu [menuItem]
  setPosition menu (0.0,0.0)
  (width, _) <- getWinSize
  setPosition menuItem (width - 50.0, 25.)
  addChild scene menu

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
  l <- layerGradient        

data Test = Test {
                  title     :: String,
                  testScene :: IO Scene
                 }

tests :: [Test]
tests = [
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
