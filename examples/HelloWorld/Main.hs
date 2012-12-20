module Main where

import Cocos2d

main = cocos2dApp $ cocosMain
cocosMain app = do
  director <- getDirectorInstance
  -- display FPS
  setDisplayStats director True
  -- set the animation interval
  setAnimationInterval director $ 1.0 / 60.0
  -- create the scene
  scene <- helloWorldScene
  runWithScene director scene

helloWorldScene :: IO Scene
helloWorldScene = do
  s <- createScene
  l <- helloWorldLayer
  addChild s l
  return s

helloWorldLayer :: IO Layer
helloWorldLayer = do
  l       <- createLayer
  (width,height) <- getWinSize
  -- Add menu item with "X" image, which is clicked to quit the program
  closeItem <- createMenuItemImage "res/CloseNormal.png" "res/CloseSelected.png" quit 
  setAnchorPoint closeItem (0.5, 0.5)
  menu <- createMenuWithItems [closeItem]
  pointZero >>= setPosition menu
  addChild l menu
  setPosition closeItem (width - 20.0, 20.0)
  
  -- Add a label shows "hello World"
  helloLabel <- createLabelTTF "Hello World" "Arial" 38
  setPosition helloLabel (width /2.0, 0.0)
  addChild l helloLabel

  -- hello world splash screen
  sprite <- createSprite "res/HelloWorld.png"
  setPosition sprite (width /2.0, height /2.0)
  setScale sprite 0.5
  setRotation sprite 180.0
  addChild l sprite

  -- Animate the splash screen
  runAction sprite $ Sequence [RotateTo 2.0 0.0, ScaleTo 2.0 (1.0,1.0)]
  runAction helloLabel $ MoveBy 2.5 (0.0, height-40.0)

  return l
  
