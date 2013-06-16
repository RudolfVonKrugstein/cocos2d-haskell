module MainMenu (
  addMainMenuMenu,
  goToMainMenu,
  mainMenuScene,
  runTestCase
) where

import Graphics.Cocos2d
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Resources
import Tests
import TestCase

line_SPACE = 40.0


-- add main menu to scene
addMainMenuMenu :: Scene -> IO ()
addMainMenuMenu scene = do
  label    <- createLabelTTF "Main Menu" "Arial" 20
  menuItem <- createMenuItemLabel label goToMainMenu
  menu     <- createMenuWithItems [toMenuItem menuItem]
  setPosition menu (0.0,0.0)
  setZOrder menu (1)
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
                              menuItem <- createMenuItemLabel label (runTestCase (testCase t))
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

-- run a test case, placed in this module to break circular dependencies
runTestCase :: TestCase -> IO ()
runTestCase test = do
  (winWidth,winHeight) <- getWinSize
  scene <- scene test
  addMainMenuMenu scene

  label <- createLabelTTF (title test) "Arial" 32
  addChild scene label 1
  setPosition label (winWidth/2.0, winHeight -50.0)

  item1 <- createMenuItemImage s_pathB1 s_pathB2 (runTestCase (prev test))
  item2 <- createMenuItemImage s_pathR1 s_pathR2 (runTestCase test)
  item3 <- createMenuItemImage s_pathF1 s_pathF2 (runTestCase (next test))

  menu <- createMenuWithItems $ [toMenuItem item1,toMenuItem item2,toMenuItem item3]

  setPosition menu (0.0,0.0)
  (width2,height2) <- getContentSize item2

  setPosition item1 (winWidth /2.0 - width2 *2.0, height2 /2.0)
  setPosition item2 (winWidth/2.0, height2 /2.0)
  setPosition item3 (winWidth/2.0 + width2 * 2.0, height2 /2.0)

  addChild scene menu 1

  replaceScene scene
