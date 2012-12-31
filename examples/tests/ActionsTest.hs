module ActionsTest where

import Graphics.Cocos2d
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Graphics.Cocos2d.Animation
import Resources
import TestCase
import Data.VectorSpace
import Data.IORef
-- based on ActionsTest.js from cocos2d-html5

actionsTestCase = testCaseFromList actionTests

actionTests = [
    (actionManualScene,"Action Test"),
    (actionMoveScene, "Action Test"),
    (actionScaleScene, "Action Test"),
    (actionRotateScene, "Action Test"),
    (actionSkewScene, "Action Test"),
    (actionSkewRotateScaleScene, "Action Test"),
    (actionJumpScene, "Action Test"),
    (actionBezierScene, "Action Test"),
    (actionCardinalSplineScene, "Action Test"),
    (actionCatmullRomScene, "Action Test"),
    (actionBlinkScene, "Action Test"),
    (actionFadeScene, "Action Test"),
    (actionTintScene, "Action Test"),
    (actionSequenceScene, "Action Test"),
    (actionSequence2Scene, "Action Test"),
    (actionSpawnScene, "Action Test"),
    (actionReverseScene, "Action Test"),
    (actionDelayTimeScene, "Action Test"),
    (actionRepeatScene, "Action Test"),
    (actionRepeatForeverScene, "Action Test"),
    (actionRotateToRepeatScene, "Action Test"),
    (actionRotateJerkScene, "Action Test"),
    --(actionCallFunc1Scene, "Action Test"),
    (actionCallFunc2Scene, "Action Test"),
    --(actionCallFunc3Scene, "Action Test"),
    (actionReverseSequenceScene, "Action Test"),
    (actionReverseSequence2Scene, "Action Test"),
    (actionOrbitScene, "Action Test"),
    (actionFollowScene, "Action Test"),
    (actionTargetedScene, "Action Test"),
    (actionTargetedCopyScene, "Action Test"),
    (pauseResumeActionsScene, "Action Test")
    ]

-- Sprites used all the time
data Sprites = Sprites {
  _grossini :: Sprite,
  _tamara :: Sprite,
  _kathia :: Sprite
}

toList :: Sprites -> [Sprite]
toList sprites = [_grossini sprites, _tamara sprites, _kathia sprites]

-- centers an shows the requested number of sprites
centerSprites :: Sprites -> Int -> IO ()
centerSprites s n = do
  let sprites = toList s
      num = fromIntegral $ min (length sprites) n :: Double
  -- hide all but the first n sprites
  mapM_ (\s -> setVisible s False) (drop n sprites)
  -- set the first nums position
  (winWidth, winHeight) <- getWinSize
  mapM_ (\(i,s) -> setPosition s (i * winWidth / (1.0 + num), winHeight /2.0)) $ zip [1.0..] (take n sprites)

-- Same but with left aligning
alignSpritesLeft :: Sprites -> Int -> IO ()
alignSpritesLeft s n = do
  let sprites = toList s
  -- hide all but the first n sprites
  mapM_ (\s -> setVisible s False) (drop n sprites)
  (winWidth, winHeight) <- getWinSize
  let yPos = case n of
                1 -> [winHeight/2.0]
                2 -> [winHeight/3.0, 2.0 * winHeight/3.0]
                3 -> [winHeight/2.0, 2.0 * winHeight/3.0, winHeight/3.0]
  mapM_ (\(y,s) -> setPosition s (60.0, y)) $ zip yPos sprites

-- load the standard sprites and add them to the scene
loadAndAddSprites :: Layer -> IO Sprites
loadAndAddSprites layer = do
  _grossini <- createSprite s_pathGrossini
  _tamara   <- createSprite s_pathSister1
  _kathia   <- createSprite s_pathSister2
  addChild layer _grossini 1
  addChild layer _tamara 2
  addChild layer _kathia 3
  (winWidth, winHeight) <- getWinSize
  setPosition _grossini (winWidth / 2.0, winHeight/ 3.0)
  setPosition _tamara (winWidth / 2.0, 2* winHeight/ 3.0)
  setPosition _kathia (winWidth / 2.0, winHeight/ 2.0)
  return $ Sprites _grossini _tamara _kathia

-- add a subtitle to a scene
addSubtitle :: Layer -> String -> IO ()
addSubtitle layer subtitle = do
  (winWidth, winHeight) <- getWinSize
  l <- createLabelTTF subtitle "Thonburi" 22
  addChild layer l 1
  setPosition l (winWidth/2.0, winHeight-60.0)

addCode :: Layer -> String -> IO ()
addCode layer code = do
  (winWidth, winHeight) <- getWinSize
  label <- createLabelTTF code "Thonburi" 16
  setPosition label (winWidth/2.0, winHeight-120.0)
  addChild layer label 10
  labelBG <- createLabelTTF code "Thonburi" 16
  setColor labelBG (Color4b 10 10 255 255)
  setPosition labelBG (winWidth/2.0+1.0, winHeight-120.0-1.0)
  addChild layer labelBG 9

-- Layer used for action demo
createActionDemoLayer :: (Maybe String) -> (Maybe String) -> IO Layer
createActionDemoLayer subtitle code = do
  l <- createLayerGradient (Color4b 0 0 0 255) (Color4b 98 99 117 255)
  case subtitle of
    Just s -> addSubtitle l s
    _      -> return ()
  case code of
    Just c -> addCode l c
    _      -> return ()
  return l

actionDemoScene :: (Maybe String) -> (Maybe String) -> (Sprites -> IO ()) -> IO Scene
actionDemoScene subtitle code run = createScene $ \scene -> do
  layer <- createActionDemoLayer subtitle code
  sprites <- loadAndAddSprites layer
  addChild_ scene layer
  run sprites
   

------------------------------------------------------------------
-- ActionManual
------------------------------------------------------------------

actionManualScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  setScale (_tamara sprites) (2.5, -1.0)
  setPosition (_tamara sprites) (100.0, 70.0)
  setOpacity (_tamara sprites) 128.0

  setRotation (_grossini sprites) 120.0
  setPosition (_grossini sprites) (winWidth/2.0, winHeight/2.0)
  setColor (_grossini sprites) (Color4b 255 0 0 255)

  setPosition (_kathia sprites) (winWidth - 100.0, winHeight/2.0)
  setColor (_kathia sprites) (Color4b 0 0 255 255)
  where
    code = Just "sprite.setPosition( 10,20 );\nsprite.setRotation( 90 );\nsprite.setScale( 2 );"
    subtitle = Just "Manual Transformation"

------------------------------------------------------------------
--	ActionMove
------------------------------------------------------------------

actionMoveScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3 
  
  let moveTo = MoveTo 2.0 (winWidth - 400, winHeight - 40.0)
      moveBy = MoveBy 20 (80.0, 80.0)
  runAction (_tamara sprites) moveTo
  runAction (_grossini sprites) (Sequence [moveBy, Reverse moveBy])
  runAction (_kathia sprites) (MoveTo 1.0 (40.0,40.0))
  where
    code = Just "a = cc.MoveBy.create( time, cc.p(x,y) );\na = cc.MoveTo.create( time, cc.p(x,y) );"
    subtitle = Just "MoveTo / MoveBy";

------------------------------------------------------------------
-- ActionScale
------------------------------------------------------------------

actionScaleScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3

  let actionTo  = ScaleTo 2 (0.5, 0.5)
      actionBy  = ScaleBy 2.0 (2.0, 2.0)
      actionBy2 = ScaleBy 2.0 (0.25, 4.5)

  runAction (_tamara sprites) actionTo
  runAction (_kathia sprites) (Sequence [actionBy2, Reverse actionBy2])
  runAction (_grossini sprites) (Sequence [actionBy, Reverse actionBy])
  where
   code = Just "a = cc.ScaleBy.create( time, scale );\na = cc.ScaleTo.create( time, scaleX, scaleY );"
   subtitle = Just "ScaleTo / ScaleBy"

------------------------------------------------------------------
--	ActionSkew
------------------------------------------------------------------

actionSkewScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3

  let actionTo = SkewTo 2.0 (37.2, -372)
      actionToBack = SkewTo 2.0 (0.0, 0.0)
      actionBy = SkewBy 2.0 (0.0, -90.0)
      actionBy2 = SkewBy 2.0 (45.0,45.0)

  runAction (_tamara sprites) (Sequence [actionTo,actionToBack])
  runAction (_grossini sprites) (Sequence [actionBy, Reverse actionBy])
  runAction (_kathia sprites) (Sequence [actionBy2, Reverse actionBy2])

  where code = Just "a = cc.SkewBy.create( time, skew );\na = cc.SkewTo.create( time, skewX, skewY );"
        subtitle = Just "SkewTo / SkewBy"

actionSkewRotateScaleScene = createScene $ \scene -> do
  (winWidth, winHeight) <- getWinSize
  layer <- createActionDemoLayer subtitle code
  sprites <- loadAndAddSprites layer
  addChild_ scene layer

  let boxSize = (100.0, 100.0) :: (Double,Double)
  box <- createLayerColor (Color4b 255 255 0 255)
  setAnchorPoint box (0.0, 0.0)
  setPosition box $ (0.5 :: Double) *^ ((winWidth,winHeight) ^-^ boxSize)
  setContentSize box boxSize

  let markrside = 10.0
  uL <- createLayerColor (Color4b 255 0 0 255)
  addChild_ box uL
  setContentSize uL (markrside,markrside)
  setPosition uL (0.0, (snd boxSize) - markrside)
  setAnchorPoint uL (0.0, 0.0)

  uR <- createLayerColor (Color4b 0 0 255 255)
  addChild_ box uR
  setContentSize uR (markrside,markrside)
  setPosition uR ((fst boxSize) + markrside, (snd boxSize) - markrside)
  setAnchorPoint uL (0.0, 0.0)

  addChild_ layer box
  
  let actionTo = SkewTo 2.0 (0.0, 2.0)
      rotateTo = RotateTo 2.0 61.0
      actionScaleTo = ScaleTo 2.0 (-0.44, 0.47)
      actionScaleToBack = ScaleTo 2.0 (1.0, 1.0)
      rotateToBack = RotateTo 2.0 0.0
      actionToBack = SkewTo 2.0 (0, 0)

  runAction box (Sequence [actionTo, actionToBack])
  runAction box (Sequence [rotateTo, rotateToBack])
  runAction box (Sequence [actionScaleTo, actionScaleToBack])

  where code = Nothing
        subtitle = Just "Skew + Rotate + Scale"

------------------------------------------------------------------
--	ActionRotate
------------------------------------------------------------------

actionRotateScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3

  let actionTo = RotateTo 2.0 45.0
      actionTo2 = RotateTo 2.0 (-45.0)
      actionTo0 = RotateTo 2.0 0.0
  runAction (_tamara sprites) (Sequence [actionTo, actionTo0])
  let actionBy = RotateBy 2.0 360.0
      actionByBack = Reverse actionBy
  runAction (_grossini sprites) (Sequence [actionBy, actionByBack])

  runAction (_kathia sprites) (Sequence [actionTo2, actionTo0])
  where code = Just "a = cc.RotateBy.create( time, degrees );\na = cc.RotateTo.create( time, degrees );"
        subtitle = Just "RotateTo / RotateBy"

------------------------------------------------------------------
-- ActionJump
------------------------------------------------------------------

actionJumpScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3
  let actionTo = JumpTo 2.0 (300.0, 300.0) 50.0 4
      actionBy = JumpBy 2.0 (300.0, 0.0) 50.0 4
      actionUp = JumpBy 2.0 (0.0, 0.0) 80.0 4
      actionByBack = Reverse actionBy

  runAction (_tamara sprites) actionTo
  runAction (_grossini sprites) (Sequence [actionBy,actionByBack])
  runAction (_kathia sprites)   (RepeatForever actionUp)
  where code = Just "a = cc.JumpBy.create( time, point, height, #_of_jumps );\na = cc.JumpTo.create( time, point, height, #_of_jumps );"
        subtitle = Just "JumpTo / JumpBy"

------------------------------------------------------------------
-- ActionBezier
------------------------------------------------------------------

actionBezierScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  -- startPosition can be any coordinate, but since the movement
  -- is relative to the Bezier curve, make it (0,0)
  centerSprites sprites 3

  -- sprite 1

  -- 3 and only 3 control points should be used for Bezier actions.
  let controlPoints = (
		  (0.0, winHeight/2.0),
		  (300.0, -winHeight/2.0),
		  (300.0, 100.0)
		  )

      bezierForward = BezierBy 3.0 controlPoints
      rep = RepeatForever $ Sequence [bezierForward, Reverse $ bezierForward]

  -- sprite 2
  setPosition (_tamara sprites) (80.0, 160.0)

  -- 3 and only 3 control points should be used for Bezier actions.
  let controlPoints2 = (
		  (100.0, winHeight / 2.0),
		  (200.0, -winHeight / 2.0),
		  (240.0, 160.0)
		  )
      bezierTo1 = BezierTo 2.0 controlPoints2

  -- // sprite 3
      controlPoints3 = controlPoints2
  setPosition (_kathia sprites) (400.0, 160.0)
  let bezierTo2 = BezierTo 2.0 controlPoints3

  runAction (_grossini sprites) rep
  runAction (_tamara sprites) bezierTo1
  runAction (_kathia sprites) bezierTo2
  where code = Nothing
        subtitle = Just "BezierBy / BezierTo"

------------------------------------------------------------------
-- Issue1008
--------------------------------------------------------------------

actionIssue1008Scene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 1

  -- sprite 1
  setPosition (_grossini sprites) (428.0,279.0)

  -- 3 and only 3 control points should be used for Bezier actions.
  let controlPoints1 = ((428.0,279.0), (100.0,100.0), (100.0,100.0))
      controlPoints2 = ((100.0,100.0), (428.0,279.0), (428.0,279.0))

      bz1 = BezierTo 3.0 controlPoints1
      bz2 = BezierTo 3.0 controlPoints2
      trace = CallFunc (onTrace $ _grossini sprites)

      rep = RepeatForever $ Sequence [bz1, bz2, trace]

  runAction (_grossini sprites) rep
  where code = Nothing
        subtitle = Just"BezierTo + Repeat. See console";

onTrace :: Sprite -> IO ()
onTrace sprite = do
  (x,y) <- getPosition sprite
  logOut $ "Position x:" ++ (show x) ++ " y:" ++ (show y)
  if x /= 428.0 || y /= 279.0 then
    logOut "Error: Issue 1008 is still open"
  else
    return ()
------------------------------------------------------------------
-- ActionBlink
------------------------------------------------------------------

actionBlinkScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 3

  let action1 = Blink 2.0 10
      action2 = Blink 2.0 5

  runAction (_tamara sprites) action1
  runAction (_kathia sprites) action2
  where code = Just "a = cc.Blink.create( time, #_of_blinks );"
        subtitle = Just "Blink"
------------------------------------------------------------------
-- ActionFade
------------------------------------------------------------------

actionFadeScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize

  centerSprites sprites 2
  setOpacity (_tamara sprites) 0

  let action1 = FadeIn 1.0
      action1Back = Reverse action1
      action2 = FadeOut 1.0
      action2Back = Reverse action2

  runAction (_tamara sprites) $ Sequence [action1, action1Back]
  runAction (_kathia sprites) $ Sequence [action2, action2Back]
  where code = Just "a = cc.FadeIn.create( time );\na = cc.FadeOut.create( time );"
        subtitle = Just "Blink"

------------------------------------------------------------------
-- ActionTint
------------------------------------------------------------------
actionTintScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 2

  let action1 = TintTo 2.0 $ Color4b 255 0 255 255
      action2 = TintBy 2.0 $ DeltaColor4b (-127) (-255) (-127) 0
      action2Back = Reverse $ action2

  runAction (_tamara sprites) action1
  runAction (_kathia sprites) $ Sequence [action2, action2Back]
  
  where code = Just "a = cc.TintBy.create( time, red, green, blue );\na = cc.TintTo.create( time, red, green, blue );"
        subtitle = Just "TintTo / TintBy";

------------------------------------------------------------------
-- ActionAnimate
------------------------------------------------------------------
actionAnimateScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 3
  --
  -- Manual animation
  -- 
  let frameNames = map (\i -> "res/Images/grossini_dance_" ++ (show2DigitInt i) ++ ".png") [1..15]
      animation = ImageFileAnimation frameNames (2.8/14.0) True

  let action = AnimationAction animation
  runAction (_grossini sprites) $ Sequence [action, Reverse $ action]

  -- 
  -- File animation
  -- With 2 loops and reverse
  loadAnimationsIntoCache s_animations2Plist
  let animation2 = CacheAnimation "dance_1"
      action2 = AnimationAction animation2
  runAction (_tamara sprites) $ Sequence [action2, Reverse $ action2]

  --
  -- File animation
  --
  -- with 4 loops
  let animation3 = Loop 4 animation2
      action3 = AnimationAction animation3
  runAction (_kathia sprites) action3

  where
    subtitle = Just "Center: Manual animation. Border: using file format animation"
    code = Nothing
    show2DigitInt i = if i < 10 then '0':(show i) else (show i)


------------------------------------------------------------------
--	ActionSequence
------------------------------------------------------------------
actionSequenceScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  alignSpritesLeft sprites 1

  let action = Sequence [MoveBy 2.0 (240.0,0.0), RotateBy 2.0 540.0]
  runAction (_grossini sprites) action
  where
    code = Just "a = cc.Sequence.create( a1, a2, a3,..., aN);"
    subtitle = Just "Sequence: Move + Rotate"
------------------------------------------------------------------
--	ActionSequence2
------------------------------------------------------------------
actionSequence2Scene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 1
  setVisible (_grossini sprites) False
  layer <- getParent (_grossini sprites)
  let action = Sequence [Place (200.0, 200.0), Show, MoveBy 1.0 (100.0, 0.0), CallFunc $ callback1 layer, CallFunc $ callback2 layer, CallFunc $ callback3 layer]
  runAction (_grossini sprites) action
  where
    code = Nothing
    subtitle = Just "Sequence of InstantActions"
    callback1 layer = do
      (winWidth, winHeight) <- getWinSize
      label <- createLabelTTF "callback 1 called" "Marker Felt" 16
      setPosition label (winWidth /4.0, winHeight/2.0)
      addChild_ layer label
    callback2 layer = do
      (winWidth, winHeight) <- getWinSize
      label <- createLabelTTF "callback 2 called" "Marker Felt" 16
      setPosition label (winWidth/4.0 * 2.0, winHeight / 2.0)
      addChild_ layer label
    callback3 layer = do
      (winWidth, winHeight) <- getWinSize
      label <- createLabelTTF "callback 3 called" "Marker Felt" 16
      setPosition label (winWidth/4.0 * 3.0, winHeight / 2.0)
      addChild_ layer label
      
------------------------------------------------------------------
-- ActionCallFunc2
------------------------------------------------------------------
actionCallFunc2Scene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 1
  layer <- getParent (_grossini sprites)
  let action = Sequence [MoveBy 2.0 (200.0, 0.0),
                         CallFunc $ removeFromParentAndCleanup (_grossini sprites)]
  runAction (_grossini sprites) action

  where
    code = Nothing
    subtitle = Just "CallFunc + removeFromParentAndCleanup. Grossini dissapears in 2s"
    removeFromParentAndCleanup s = removeFromParent s


------------------------------------------------------------------
-- ActionSpawn
------------------------------------------------------------------

actionSpawnScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  alignSpritesLeft sprites 1

  let action = Spawn [JumpBy 2.0 (300.0, 0.0) 50.0 4, RotateBy 2.0 270.0]
  runAction (_grossini sprites) action

  where
    code = Just "a = cc.Spawn.create( a1, a2, ..., aN );"
    subtitle = Just "Spawn: Jump + Rotate"

------------------------------------------------------------------
-- ActionRepeatForever
------------------------------------------------------------------

actionRepeatForeverScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 1
  layer <- getParent (_grossini sprites)
  let action = Sequence [DelayTime 1.0, CallFunc $ repeatForever layer]
  runAction (_grossini sprites) action
  where
    code = Just "a = cc.RepeatForever.create( action_to_repeat );"
    subtitle = Just "CallFuncN + RepeatForever"
    repeatForever sender = do
      let repeat = RepeatForever $ RotateBy 1.0 360.0
      runAction sender repeat
    
------------------------------------------------------------------
-- ActionRotateToRepeat
------------------------------------------------------------------

actionRotateToRepeatScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 2
  let act1 = RotateTo 1.0 90.0
      act2 = RotateTo 1.0 0.0
      seq  = Sequence [act1,act2]
      rep1 = RepeatForever seq
      rep2 = Repeat seq 10
  runAction (_tamara sprites) rep1
  runAction (_kathia sprites) rep2
  where
    code = Just "a = cc.Repeat.create( action_to_repeat, #_of_times );"
    subtitle = Just  "Repeat/RepeatForever + RotateTo"
------------------------------------------------------------------
-- ActionRotateJerk
------------------------------------------------------------------
actionRotateJerkScene = actionDemoScene subtitle code $ \sprites -> do
  (winWidth, winHeight) <- getWinSize
  centerSprites sprites 2
  let seq = Sequence [RotateTo 0.5 (-20.0), RotateTo 0.5 20.0]
      rep1 = Repeat seq 10
      rep2 = RepeatForever seq
  runAction (_tamara sprites) rep1
  runAction (_kathia sprites) rep2
  where
    code = Nothing
    subtitle = Just "RepeatForever / Repeat + Rotate"
------------------------------------------------------------------
-- ActionReverse
------------------------------------------------------------------
actionReverseScene = actionDemoScene subtitle code $ \sprites -> do
  alignSpritesLeft sprites 1
  let jump = JumpBy 2.0 (300.0, 0.0) 50.0 4
      action = Sequence [jump, Reverse jump]
  runAction (_grossini sprites) action
  where
    code = Just "a = action.reverse();"
    subtitle = Just "Reverse an action"
------------------------------------------------------------------
-- ActionDelayTime
------------------------------------------------------------------
actionDelayTimeScene = actionDemoScene subtitle code $ \sprites -> do
  alignSpritesLeft sprites 1
  let move = MoveBy 1.0 (150.0, 0.0)
      action = Sequence [move, DelayTime 2.0, move]
  runAction (_grossini sprites) action
  where
    code = Just "a = cc.DelayTime.create( time );"
    subtitle = Just "DelayTime: m + delay + m"
------------------------------------------------------------------
-- ActionReverseSequence
------------------------------------------------------------------
actionReverseSequenceScene = actionDemoScene subtitle code $ \sprites -> do
  alignSpritesLeft sprites 1
  let move1 = MoveBy 1.0 (250.0,0.0)
      move2 = MoveBy 1.0 (0.0,50.0)
      seq = Sequence [move1,move2,Reverse move1]
      action = Sequence [seq,Reverse seq]

  runAction (_grossini sprites) action
  where
    code = Nothing
    subtitle = Just "Reverse a sequence"

------------------------------------------------------------------
-- ActionReverseSequence2
------------------------------------------------------------------
actionReverseSequence2Scene = actionDemoScene subtitle code $ \sprites -> do
  alignSpritesLeft sprites 2

  --  Test:
  --   Sequence should work both with IntervalAction and InstantActions
  let move1 = MoveBy 3.0 (250.0, 0.0)
      move2 = MoveBy 3.0 (0.0, 50.0)
      tog1  = ToggleVisibility
      tog2  = ToggleVisibility
      seq   = Sequence [move1, tog1, move2, tog2, Reverse move1]
      action = Repeat (Sequence [seq, Reverse $ seq]) 3

  -- Test:
  --  Also test that the reverse of Hide is Show, and vice-versa
  runAction (_kathia sprites) action

  let move_tamara = MoveBy 1.0 (100.0, 0.0)
      move_tamara2 = MoveBy 1.0 (50.0, 0.0)
      hide = Hide
      seq_tamara = Sequence [move_tamara, hide, move_tamara2]
      seq_back = Reverse $ seq_tamara
  runAction (_tamara sprites) $ Sequence [seq_tamara, seq_back]
  where
    code = Nothing
    subtitle = Just "Reverse sequence 2";

------------------------------------------------------------------
-- ActionRepeat
------------------------------------------------------------------

actionRepeatScene = actionDemoScene subtitle code $ \sprites -> do
  alignSpritesLeft sprites 2
  let a1 = MoveBy 1.0 (150.0, 0.0)
      action1 = Repeat (Sequence [Place (60.0, 60.0), a1]) 3
      action2 = RepeatForever $ Sequence [a1, Reverse a1]
  runAction (_kathia sprites) action1
  runAction (_tamara sprites) action2
  where
    code = Nothing
    subtitle = Just "Repeat / RepeatForever actions"

------------------------------------------------------------------
-- ActionOrbit
------------------------------------------------------------------

actionOrbitScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 3
  let orbit1 = OrbitCamera 2.0 1.0 0.0 0.0 180.0 0.0 0.0
      action1 = Sequence [ orbit1, Reverse orbit1 ]
      
      orbit2 = OrbitCamera 2.0 1.0 0.0 0.0 180.0 (-45.0) 0.0
      action2 = Sequence [orbit2, Reverse orbit2]
 
      orbit3 = OrbitCamera 2.0 1.0 0.0 0.0 180.0 90.0 0.0
      action3 = Sequence [orbit3, Reverse orbit3]

  runAction (_kathia sprites) (RepeatForever action1)
  runAction (_tamara sprites) (RepeatForever action2)
  runAction (_grossini sprites) (RepeatForever action3)
  
  let move = MoveBy 3.0 (100.0, -100.0)
      move_back = Reverse move
      seq = Sequence [move, move_back]
      rfe = RepeatForever seq
  runAction (_kathia sprites) rfe
  runAction (_tamara sprites) rfe
  runAction (_grossini sprites) rfe

  where
    code = Nothing
    subtitle = Just "OrbitCamera action"

------------------------------------------------------------------
-- ActionFollow
------------------------------------------------------------------
actionFollowScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 1
  (winWidth, winHeight) <- getWinSize

  setPosition (_grossini sprites) ((-winWidth/2.0),(winHeight/2.0))
  let move = MoveBy 2.0 (winWidth * 3.0, 0.0)
      move_back = Reverse move
      seq = Sequence [move, move_back]
      rep = RepeatForever seq
  runAction (_grossini sprites) rep
  layer <- getParent (_grossini sprites)
  runAction layer $ Follow (toNode . _grossini $ sprites) 0.0 0.0 (winWidth * 2.0 - 100.0) winHeight
  where
    code = Nothing
    subtitle = Just "Follow action"

------------------------------------------------------------------
-- ActionCardinalSpline
------------------------------------------------------------------
actionCardinalSplineScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 2
  (winWidth, winHeight) <- getWinSize
  let array = [(0.0, 0.0),
               (winWidth /2.0 - 30.0, 0.0),
               (winWidth /2.0 - 30.0, winHeight - 80.0),
               (0.0, winHeight - 80.0),
               (0.0, 0.0)] 

  -- sprite 1 (By)
  --
  -- Spline with no tension (tension==0)
  let action1 = CardinalSplineBy 4.0 array 0.0
      reverse1 = Reverse action1
      seq = Sequence [action1,reverse1]
  setPosition (_tamara sprites) (50.0, 50.0)
  runAction (_tamara sprites) seq

  --
  -- sprite 2 (By)
  --
  -- Spline with high tension (tension==1)
  let action2 = CardinalSplineBy 3.0 array 1
      reverse2 = Reverse action2
      seq2 = Sequence [action2, reverse2]
  setPosition (_kathia sprites) (winWidth / 2.0, 50.0)
  runAction (_kathia sprites) seq2

  where
    code = Just " a = cc.CadinalSplineBy.create( time, array_of_points, tension );\n a = cc.CadinalSplineTo.create( time, array_of_points, tension );"
    subtitle = Just "Cardinal Spline paths. Testing different tensions for one array"

------------------------------------------------------------------
-- ActionCatmullRom
------------------------------------------------------------------
actionCatmullRomScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 2
  (winWidth, winHeight) <- getWinSize

  -- sprite 1 (By)
  -- 
  -- startPosition can be any coordinate, but since the movement
  -- is relative to the Catmull Rom curve, it is better to start with (0,0).
  --
  setPosition (_tamara sprites) (50.0, 50.0)
  let array = [(0.0, 0.0),
               (80.0, 80.0),
               (winWidth - 80.0, 80.0),
               (winWidth - 80.0, winHeight - 80.0),
               (80.0, winWidth - 80.0),
               (80.0, 80.0),
               (winWidth /2.0, winHeight/2.0)]
      action1 = CatmullRomBy 3.0 array
      reverse1 = Reverse action1
      seq1 = Sequence [action1, reverse1]
  runAction (_tamara sprites) seq1

  -- 
  -- sprite 2 (To)
  -- 
  -- The startPosition is not important here, because it uses a "To" action.
  -- The initial position will be the 1st point of the Catmull Rom path
  --
  let array2 = [(winWidth/2.0, 30.0),
                (winWidth - 80.0, 30.0),
                (winWidth - 80.0, winHeight - 80.0),
                (winWidth / 2.0, winHeight - 80.0),
                (winWidth / 2.0, 30.0)]
      action2 = CatmullRomTo 3.0 array2
      reverse2 = Reverse action2
      
      seq2 = Sequence [action2, reverse2]
  runAction (_kathia sprites) seq2
  where
    code = Just "a = cc.CatmullRomBy.create( time, array_of_points );\n a = cc.CatmullRomTo.create( time, array_of_points );"
    subtitle = Just "Catmull Rom spline paths. Testing reverse too"

------------------------------------------------------------------
-- ActionTargeted
------------------------------------------------------------------
actionTargetedScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 2
  (winWidth, winHeight) <- getWinSize
  let jump1 = JumpBy 2.0 (0.0,0.0) 100.0 3
      jump2 = jump1
      rot1 = RotateBy 1.0 360.0
      rot2 = rot1
     
      t1 = TargetedAction (toNode . _kathia $ sprites) jump2
      t2 = TargetedAction (toNode . _kathia $ sprites) rot2

      seq = Sequence [jump1, t1, rot1, t2]
      always = RepeatForever seq
  runAction (_tamara sprites) always
  where
    code = Just "a = cc.TargetedAction.create( target, action );"
    subtitle = Just "ActionTargeted"

------------------------------------------------------------------
-- ActionTargetedCopy
------------------------------------------------------------------
actionTargetedCopyScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 2
  (winWidth, winHeight) <- getWinSize
  let jump1 = JumpBy 2.0 (0.0,0.0) 100.0 3
      jump2 = jump1
      
      t1 = TargetedAction (toNode . _kathia $ sprites) jump2
      t_copy = t1

      seq = Sequence [jump1, t_copy]

  runAction (_tamara sprites) seq
  where
    code = Nothing
    subtitle = Just "Testing copy on TargetedAction"

------------------------------------------------------------------
-- PauseResumeActions
------------------------------------------------------------------
pauseResumeActionsScene = actionDemoScene subtitle code $ \sprites -> do
  centerSprites sprites 2
  (winWidth, winHeight) <- getWinSize

  runAction (_tamara sprites) . RepeatForever $ RotateBy 3.0 360.0
  runAction (_grossini sprites) . RepeatForever $ RotateBy 3.0 (-360.0)
  runAction (_kathia sprites) . RepeatForever $ RotateBy 3.0 360.0
 
  layer <- getParent (_grossini sprites)

  pausedTargets <- newIORef ([] :: [Node])
  scheduleOnce layer (pause pausedTargets) 3.0
  scheduleOnce layer (resume pausedTargets) 5.0

  where
    code = Nothing
    subtitle = Just "all actions pause at 3s and resume at 5s"
    pause ps = do
        logOut "Pausing"
        ns <- pauseAllRunningActions
        writeIORef ps ns

    resume ps = do
        ns <- readIORef ps
        logOut "Resuming"
        resumeTargets ns

