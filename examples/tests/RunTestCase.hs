
runTestCase :: TestCase -> (Scene -> IO ()) -> IO ()
runTestCase test modifyScene = do
  (winWidth,winHeight) <- getWinSize
  scene <- scene test
  modifyScene scene

  label <- createLabelTTF (title test) "Arial" 32
  addChild scene label 1
  setPosition label (winWidth/2.0, winHeight -50.0)

  item1 <- createMenuItemImage s_pathB1 s_pathB2 (runTestCase (prev test) modifyScene)
  item2 <- createMenuItemImage s_pathR1 s_pathR2 (runTestCase test modifyScene)
  item3 <- createMenuItemImage s_pathF1 s_pathF2 (runTestCase (next test) modifyScene)

  menu <- createMenuWithItems $ [toMenuItem item1,toMenuItem item2,toMenuItem item3]

  setPosition menu (0.0,0.0)
  (width2,height2) <- getContentSize item2

  setPosition item1 (winWidth /2.0 - width2 *2.0, height2 /2.0)
  setPosition item2 (winWidth/2.0, height2 /2.0)
  setPosition item3 (winWidth/2.0 + width2 * 2.0, height2 /2.0)

  addChild scene menu 1

  replaceScene scene
