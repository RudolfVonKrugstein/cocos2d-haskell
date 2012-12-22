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
  l <- layerGradient (Color4B 0 0 0 255) (Color4B 0x46 0x82 0xB4 255)

  (winWidth, winHeight) <- getWinSize
  
  closeItem <- createMenuItemImage s_pathClose s_pathClose quit
  menu <- createMenu [closeItem]
  setPosition menu (0.0, 0.0)
  setPosition closeItem (winWIdth - 30.0, winHeight -30.0)
  addChild l menu 1
  
  items <- mapM (\(t,i) -> do label <- createLabelTTF (title t) "Arial" 24
                              menuItem <- createMenuItemLabel label (test t)
                              setPosition menuItem (winWidth /2.0, winHeight - (i + 1.0) * LINE_SPACE
                ) $ zip tests [0.0..]
  
  itemMenu = createMenu items
  addChild_ l menu

  setTouchesMoved s (onTouchesMoved menu)
  setMouseDragged s (onMouseDragged menu)
  setScrollWheel  s (onScrollWheel menu)

-- callbacks
onTouchesMoved :: Menu -> [Touch] -> IO ()
onTouchesMoved menu (t:_) = do
  let (_,y) = delta t
  moveMenu menu y

onMouseDragged :: Menu -> (Double,Double) -> IO ()
onMouseDragged menu (_,y) = moveMenu menu y

onScrollWheel :: Menu -> Double -> IO ()
onScrollWheel menu delta = moveMenu menu delta


-- move the menu
moveMenu :: Menu -> Double -> IO ()
moveMenu menu delta = do
  (_,y) <- getPosition menu
  let newY = min ((fromIntegral (length tests)) * LINE_SPACE) $ max 0.0 (y+delta)
  writeIORef yPos newY
  setPosition menu (0,newY)

data Test = Test {
                  title     :: String,
                  testScene :: IO ()
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
