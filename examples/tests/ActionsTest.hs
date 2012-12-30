module ActionsTest where

import Graphics.Cocos2d
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Action
import Graphics.Cocos2d.Animation
import Resources
import TestCase
import Data.VectorSpace
-- based on ActionsTest.js from cocos2d-html5

actionsTestCase = actionManualCase

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

actionManualCase = TestCase actionMoveCase actionManualScene actionMoveCase "ActionsTest"

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
actionMoveCase = TestCase actionManualCase actionMoveScene actionManualCase "ActionsTest"

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

actionScaleCase = TestCase actionMoveCase actionScaleScene actionSkewCase "ActionsTest"

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
actionSkewCase = TestCase actionScaleCase actionSkewScene actionSkewRotateScaleCase "ActionsTest"

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

actionSkewRotateScaleCase = TestCase actionSkewCase actionSkewRotateScaleScene actionRotateCase "ActionsTest"

actionSkewRotateScaleScene = createScene $ \scene -> do
  (winWidth, winHeight) <- getWinSize
  layer <- createActionDemoLayer subtitle code
  sprites <- loadAndAddSprites layer
  addChild_ scene layer

  let boxSize = (100.0, 100.0)
  box <- createLayerColor (Color4b 255 255 0 255)
  setAnchorPoint box (0.0, 0.0)
  setPosition box $ 0.5 *^ ((winWidth,winHeight) ^-^ boxSize)
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

actionRotateCase = TestCase actionSkewRotateScaleCase actionRotateScene actionJumpCase "ActionsTest"

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

actionJumpCase = TestCase actionRotateCase actionJumpScene undefined "ActionsTest"

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
actionBezierCase = TestCase actionJumpCase actionBezierScene undefined "ActionsTest"

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
actionIssue1008Case = TestCase actionBezierCase actionIssue1008Scene actionBlinkCase "Issue 1008"

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
actionBlinkCase = TestCase actionBezierCase actionBlinkScene undefined "ActionsTest"

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
actionFadeCase = TestCase actionBlinkCase actionFadeScene undefined "ActionsTest"

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
  let action = Sequence [MoveBy 2.0 (200.0, 0.0),
                         CallFunc $ removeFromParentAndCleanup (_grossini sprites)]
  runAction (_grossini sprites) action

  where
    code = Nothing
    subtitle = Just "CallFunc + removeFromParentAndCleanup. Grossini dissapears in 2s"
    removeFromParentAndCleanup s = removeFromParent s

{-
//------------------------------------------------------------------
//
// ActionCallFunc3
//
//------------------------------------------------------------------
var ActionCallFunc3 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(1);

        var action = cc.CallFunc.create(function(nodeExecutingAction, value) {
            cc.log("Object: " + nodeExecutingAction + " value is: " + value);
        }, this, "Hello world");

        this.runAction(action);
    },

    removeFromParentAndCleanup:function (nodeExecutingAction, data) {
        nodeExecutingAction.removeFromParent(data);
    },

    title:function () {
        return "CallFunc + parameters";
    },
    subtitle:function () {
        return "CallFunc + parameters. Take a look at the console";
    }
});

//------------------------------------------------------------------
//
// ActionSpawn
//
//------------------------------------------------------------------
var ActionSpawn = ActionsDemo.extend({

    _code:"a = cc.Spawn.create( a1, a2, ..., aN );",

    onEnter:function () {
        this._super();
        this.alignSpritesLeft(1);

        var action = cc.Spawn.create(
            cc.JumpBy.create(2, cc.p(300, 0), 50, 4),
            cc.RotateBy.create(2, 720));

        this._grossini.runAction(action);

    },
    subtitle:function () {
        return "Spawn: Jump + Rotate";
    }
});
//------------------------------------------------------------------
//
// ActionRepeatForever
//
//------------------------------------------------------------------
var ActionRepeatForever = ActionsDemo.extend({
    _code:"a = cc.RepeatForever.create( action_to_repeat );",

    onEnter:function () {
        this._super();
        this.centerSprites(1);
        var action = cc.Sequence.create(
            cc.DelayTime.create(1),
            cc.CallFunc.create(this.repeatForever));    // not passing 'this' since it is not used by the callback func

        this._grossini.runAction(action);


    },
    repeatForever:function (sender) {
        var repeat = cc.RepeatForever.create(cc.RotateBy.create(1.0, 360));
        sender.runAction(repeat);
    },
    subtitle:function () {
        return "CallFuncN + RepeatForever";
    }
});
//------------------------------------------------------------------
//
// ActionRotateToRepeat
//
//------------------------------------------------------------------
var ActionRotateToRepeat = ActionsDemo.extend({
    _code:"a = cc.Repeat.create( action_to_repeat, #_of_times );",

    onEnter:function () {
        this._super();
        this.centerSprites(2);

        var act1 = cc.RotateTo.create(1, 90);
        var act2 = cc.RotateTo.create(1, 0);
        var seq = cc.Sequence.create(act1, act2);
        var rep1 = cc.RepeatForever.create(seq);
        var rep2 = cc.Repeat.create((seq.copy()), 10);

        this._tamara.runAction(rep1);
        this._kathia.runAction(rep2);

    },
    subtitle:function () {
        return "Repeat/RepeatForever + RotateTo";
    }
});
//------------------------------------------------------------------
//
// ActionRotateJerk
//
//------------------------------------------------------------------
var ActionRotateJerk = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(2);
        var seq = cc.Sequence.create(
            cc.RotateTo.create(0.5, -20),
            cc.RotateTo.create(0.5, 20));

        var rep1 = cc.Repeat.create(seq, 10);
        var rep2 = cc.RepeatForever.create((seq.copy()));

        this._tamara.runAction(rep1);
        this._kathia.runAction(rep2);
    },
    subtitle:function () {
        return "RepeatForever / Repeat + Rotate";
    }
});
//------------------------------------------------------------------
//
// ActionReverse
//
//------------------------------------------------------------------
var ActionReverse = ActionsDemo.extend({

    _code:"a = action.reverse();",

    onEnter:function () {
        this._super();
        this.alignSpritesLeft(1);

        var jump = cc.JumpBy.create(2, cc.p(300, 0), 50, 4);
        var action = cc.Sequence.create(jump, jump.reverse());

        this._grossini.runAction(action);
    },
    subtitle:function () {
        return "Reverse an action";
    }
});
//------------------------------------------------------------------
//
// ActionDelayTime
//
//------------------------------------------------------------------
var ActionDelayTime = ActionsDemo.extend({

    _code:"a = cc.DelayTime.create( time );",

    onEnter:function () {
        this._super();
        this.alignSpritesLeft(1);

        var move = cc.MoveBy.create(1, cc.p(150, 0));
        var action = cc.Sequence.create(move, cc.DelayTime.create(2), move);

        this._grossini.runAction(action);
    },
    subtitle:function () {
        return "DelayTime: m + delay + m";
    }
});
//------------------------------------------------------------------
//
// ActionReverseSequence
//
//------------------------------------------------------------------
var ActionReverseSequence = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.alignSpritesLeft(1);

        var move1 = cc.MoveBy.create(1, cc.p(250, 0));
        var move2 = cc.MoveBy.create(1, cc.p(0, 50));
        var seq = cc.Sequence.create(move1, move2, move1.reverse());
        var action = cc.Sequence.create(seq, seq.reverse());

        this._grossini.runAction(action);

    },
    subtitle:function () {
        return "Reverse a sequence";
    }
});
//------------------------------------------------------------------
//
// ActionReverseSequence2
//
//------------------------------------------------------------------
var ActionReverseSequence2 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.alignSpritesLeft(2);


        // Test:
        //   Sequence should work both with IntervalAction and InstantActions
        var move1 = cc.MoveBy.create(3, cc.p(250, 0));
        var move2 = cc.MoveBy.create(3, cc.p(0, 50));
        var tog1 = cc.ToggleVisibility.create();
        var tog2 = cc.ToggleVisibility.create();
        var seq = cc.Sequence.create(move1, tog1, move2, tog2, move1.reverse());
        var action = cc.Repeat.create(
            cc.Sequence.create(seq, seq.reverse()), 3
        );


        // Test:
        //   Also test that the reverse of Hide is Show, and vice-versa
        this._kathia.runAction(action);

        var move_tamara = cc.MoveBy.create(1, cc.p(100, 0));
        var move_tamara2 = cc.MoveBy.create(1, cc.p(50, 0));
        var hide = cc.Hide.create();
        var seq_tamara = cc.Sequence.create(move_tamara, hide, move_tamara2);
        var seq_back = seq_tamara.reverse();
        this._tamara.runAction(cc.Sequence.create(seq_tamara, seq_back));
    },
    subtitle:function () {
        return "Reverse sequence 2";
    }
});
//------------------------------------------------------------------
//
// ActionRepeat
//
//------------------------------------------------------------------
var ActionRepeat = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.alignSpritesLeft(2);


        var a1 = cc.MoveBy.create(1, cc.p(150, 0));
        var action1 = cc.Repeat.create(
            cc.Sequence.create(cc.Place.create(cc.p(60, 60)), a1),
            3);
        var action2 = cc.RepeatForever.create(
            (cc.Sequence.create((a1.copy()), a1.reverse()))
        );

        this._kathia.runAction(action1);
        this._tamara.runAction(action2);
    },
    subtitle:function () {
        return "Repeat / RepeatForever actions";
    }
});
//------------------------------------------------------------------
//
// ActionOrbit
//
//------------------------------------------------------------------
var ActionOrbit = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(3);

        var orbit1 = cc.OrbitCamera.create(2, 1, 0, 0, 180, 0, 0);
        var action1 = cc.Sequence.create(
            orbit1,
            orbit1.reverse());

        var orbit2 = cc.OrbitCamera.create(2, 1, 0, 0, 180, -45, 0);
        var action2 = cc.Sequence.create(
            orbit2,
            orbit2.reverse());

        var orbit3 = cc.OrbitCamera.create(2, 1, 0, 0, 180, 90, 0);
        var action3 = cc.Sequence.create(
            orbit3,
            orbit3.reverse());

        this._kathia.runAction(cc.RepeatForever.create(action1));
        this._tamara.runAction(cc.RepeatForever.create(action2));
        this._grossini.runAction(cc.RepeatForever.create(action3));

        var move = cc.MoveBy.create(3, cc.p(100, -100));
        var move_back = move.reverse();
        var seq = cc.Sequence.create(move, move_back);
        var rfe = cc.RepeatForever.create(seq);
        this._kathia.runAction(rfe);
        this._tamara.runAction((rfe.copy()));
        this._grossini.runAction((rfe.copy()));

    },
    subtitle:function () {
        return "OrbitCamera action";
    }
});
//------------------------------------------------------------------
//
// ActionFollow
//
//------------------------------------------------------------------
var ActionFollow = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(1);
        var s = director.getWinSize();

        this._grossini.setPosition(-(s.width/2), s.height / 2);
        var move = cc.MoveBy.create(2, cc.p(s.width * 3, 0));
        var move_back = move.reverse();
        var seq = cc.Sequence.create(move, move_back);
        var rep = cc.RepeatForever.create(seq);

        this._grossini.runAction(rep);

        this.runAction(cc.Follow.create(this._grossini, cc.rect(0, 0, s.width * 2 - 100, s.height)));
    },
    subtitle:function () {
        return "Follow action";
    }
});

//------------------------------------------------------------------
//
// ActionCardinalSpline
//
//------------------------------------------------------------------
var ActionCardinalSpline = ActionsDemo.extend({
    _array:null,

    _code:" a = cc.CadinalSplineBy.create( time, array_of_points, tension );\n" +
            " a = cc.CadinalSplineTo.create( time, array_of_points, tension );",

    ctor:function () {
        this._super();
        this._array = [];
    },

    onEnter:function () {
        this._super();

        this.centerSprites(2);

        var array = [
            cc.p(0, 0),
            cc.p(winSize.width / 2 - 30, 0),
            cc.p(winSize.width / 2 - 30, winSize.height - 80),
            cc.p(0, winSize.height - 80),
            cc.p(0, 0)
            ];

        //
        // sprite 1 (By)
        //
        // Spline with no tension (tension==0)
        //
        var action1 = cc.CardinalSplineBy.create(3, array, 0);
        var reverse1 = action1.reverse();
        var seq = cc.Sequence.create(action1, reverse1);

        this._tamara.setPosition(50, 50);
        this._tamara.runAction(seq);

        //
        // sprite 2 (By)
        //
        // Spline with high tension (tension==1)
        //
        var action2 = cc.CardinalSplineBy.create(3, array, 1);
        var reverse2 = action2.reverse();
        var seq2 = cc.Sequence.create(action2, reverse2);

        this._kathia.setPosition(winSize.width / 2, 50);
        this._kathia.runAction(seq2);

        this._array = array;
    },

    draw:function (ctx) {
        // Draw is only supported in cocos2d-html5.
        // Not supported yet on cocos2d-iphone / cocos2d-x + JSB
        this._super();

        var context = ctx || cc.renderContext;

        var apPoint = this.getAnchorPointInPoints();
        // move to 50,50 since the "by" path will start at 50,50
        context.save();
        context.translate(50 - apPoint.x ,  apPoint.y -50);
        cc.drawingUtil.drawCardinalSpline(this._array, 0, 100);
        context.restore();

        var s = director.getWinSize();

        context.save();
        context.translate(s.width / 2 - apPoint.x ,  apPoint.y - 50);
        cc.drawingUtil.drawCardinalSpline(this._array, 1, 100);
        context.restore();
    },
    subtitle:function () {
        return "Cardinal Spline paths. Testing different tensions for one array";
    },
    title:function () {
        return "CardinalSplineBy / CardinalSplineAt";
    }
});

//------------------------------------------------------------------
//
// ActionCatmullRom
//
//------------------------------------------------------------------
var ActionCatmullRom = ActionsDemo.extend({
    _array1:null,
    _array2:null,

    _code:"a = cc.CatmullRomBy.create( time, array_of_points );\n" +
            " a = cc.CatmullRomTo.create( time, array_of_points );",

    ctor:function () {
        this._super();
        this._array1 = [];
        this._array2 = [];
    },

    onEnter:function () {
        this._super();

        this.centerSprites(2);

        //
        // sprite 1 (By)
        //
        // startPosition can be any coordinate, but since the movement
        // is relative to the Catmull Rom curve, it is better to start with (0,0).
        //
        this._tamara.setPosition(50, 50);

        var array = [
                cc.p(0, 0),
                cc.p(80, 80),
                cc.p(winSize.width - 80, 80),
                cc.p(winSize.width - 80, winSize.height - 80),
                cc.p(80, winSize.height - 80),
                cc.p(80, 80),
                cc.p(winSize.width / 2, winSize.height / 2)
                ];

        var action1 = cc.CatmullRomBy.create(3, array);
        var reverse1 = action1.reverse();
        var seq1 = cc.Sequence.create(action1, reverse1);

        this._tamara.runAction(seq1);

        //
        // sprite 2 (To)
        //
        // The startPosition is not important here, because it uses a "To" action.
        // The initial position will be the 1st point of the Catmull Rom path
        //
        var array2 = [
            cc.p(winSize.width / 2, 30),
            cc.p(winSize.width - 80, 30),
            cc.p(winSize.width - 80, winSize.height - 80),
            cc.p(winSize.width / 2, winSize.height - 80),
            cc.p(winSize.width / 2, 30) ];

        var action2 = cc.CatmullRomTo.create(3, array2);
        var reverse2 = action2.reverse();

        var seq2 = cc.Sequence.create(action2, reverse2);

        this._kathia.runAction(seq2);

        this._array1 = array;
        this._array2 = array2;
    },
    draw:function (ctx) {
        // Draw is only supported in cocos2d-html5.
        // Not supported yet on cocos2d-iphone / cocos2d-x + JSB
        this._super();
        var context = ctx || cc.renderContext;

        var apPoint = this.getAnchorPointInPoints();
        // move to 50,50 since the "by" path will start at 50,50
        context.save();
        context.translate(50 - apPoint.x, apPoint.y - 50);
        cc.drawingUtil.drawCatmullRom(this._array1, 50);
        context.restore();

        context.save();
        context.translate(- apPoint.x, apPoint.y);
        cc.drawingUtil.drawCatmullRom(this._array2, 50);
        context.restore();
    },
    subtitle:function () {
        return "Catmull Rom spline paths. Testing reverse too";
    },
    title:function () {
        return "CatmullRomBy / CatmullRomTo";
    }
});

//------------------------------------------------------------------
//
// ActionTargeted
//
//------------------------------------------------------------------
var ActionTargeted = ActionsDemo.extend({
    _code:"a = cc.TargetedAction.create( target, action );",

    onEnter:function () {
        this._super();
        this.centerSprites(2);

        var jump1 = cc.JumpBy.create(2, cc.p(0,0), 100, 3);
        var jump2 = jump1.copy();
        var rot1 = cc.RotateBy.create(1, 360);
        var rot2 = rot1.copy();

        var t1 = cc.TargetedAction.create(this._kathia, jump2);
        var t2 = cc.TargetedAction.create(this._kathia, rot2);

        var seq = cc.Sequence.create(jump1, t1, rot1, t2);
        var always = cc.RepeatForever.create(seq);

        this._tamara.runAction(always);
    },
    title:function () {
        return "Action that runs on another target. Useful for sequences";
    },
    subtitle:function () {
        return "ActionTargeted";
    }
});

//------------------------------------------------------------------
//
// ActionTargetedCopy
//
//------------------------------------------------------------------
var ActionTargetedCopy = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(2);

        var jump1 = cc.JumpBy.create(2, cc.p(0,0), 100, 3);
        var jump2 = jump1.copy();

        var t1 = cc.TargetedAction.create(this._kathia, jump2);
        var t_copy = t1.copy();

        var seq = cc.Sequence.create(jump1, t_copy);

        this._tamara.runAction(seq);
    },
    title:function () {
        return "Action that runs on another target. Useful for sequences";
    },
    subtitle:function () {
        return "Testing copy on TargetedAction";
    }
});

//------------------------------------------------------------------
//
// PauseResumeActions
//
//------------------------------------------------------------------
var PauseResumeActions = ActionsDemo.extend({
    _pausedTargets:[],
    onEnter:function () {
        this._super();
        this.centerSprites(2);

        this._tamara.runAction(cc.RepeatForever.create(cc.RotateBy.create(3, 360)));
        this._grossini.runAction(cc.RepeatForever.create(cc.RotateBy.create(3, -360)));
        this._kathia.runAction(cc.RepeatForever.create(cc.RotateBy.create(3, 360)));

        this.schedule(this.pause, 3, false, 0);
        this.schedule(this.resume, 5, false, 0);
    },

    pause:function () {
        cc.log("Pausing");
        this._pausedTargets = director.getActionManager().pauseAllRunningActions();
    },
    resume:function () {
        cc.log("Resuming");
        director.getActionManager().resumeTargets(this._pausedTargets);
    },

    title:function () {
        return "PauseResumeActions";
    },
    subtitle:function () {
        return "All actions pause at 3s and resume at 5s";
    }
});

//------------------------------------------------------------------
//
// Issue1305
//
//------------------------------------------------------------------
var Issue1305 = ActionsDemo.extend({
    _spriteTemp:null,
    onEnter:function () {
        this._super();
        this.centerSprites(0);

        this._spriteTmp = cc.Sprite.create(s_pathGrossini);
        /* c++ can't support block, so we use CCCallFuncN instead.
         [spriteTmp_ runAction:[CCCallBlockN actionWithBlock:^(CCNode* node) {
         NSLog(@"This message SHALL ONLY appear when the sprite is added to the scene, NOT BEFORE");
         }] ];
         */

        this._spriteTmp.runAction(cc.CallFunc.create(this.onLog, this));
        this.scheduleOnce(this.onAddSprite, 2);
    },
    onExit:function () {
        this._super();
    },
    onLog:function (pSender) {
        cc.log("This message SHALL ONLY appear when the sprite is added to the scene, NOT BEFORE");
    },
    onAddSprite:function (dt) {
        this._spriteTmp.setPosition(250, 250);
        this.addChild(this._spriteTmp);
    },
    title:function () {
        return "Issue 1305";
    },
    subtitle:function () {
        return "In two seconds you should see a message on the console. NOT BEFORE.";
    }
});

//------------------------------------------------------------------
//
// Issue1305_2
//
//------------------------------------------------------------------
var Issue1305_2 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(0);

        var spr = cc.Sprite.create(s_pathGrossini);
        spr.setPosition(200, 200);
        this.addChild(spr);

        var act1 = cc.MoveBy.create(2, cc.p(0, 100));

        var act2 = cc.CallFunc.create(this.onLog1);
        var act3 = cc.MoveBy.create(2, cc.p(0, -100));
        var act4 = cc.CallFunc.create(this.onLog2, this);
        var act5 = cc.MoveBy.create(2, cc.p(100, -100));
        var act6 = cc.CallFunc.create(this.onLog3.bind(this));
        var act7 = cc.MoveBy.create(2, cc.p(-100, 0));
        var act8 = cc.CallFunc.create(this.onLog4, this);

        var actF = cc.Sequence.create(act1, act2, act3, act4, act5, act6, act7, act8);

        //    [spr runAction:actF];
        director.getActionManager().addAction(actF, spr, false);
    },
    onLog1:function () {
        cc.log("1st block");
    },
    onLog2:function () {
        cc.log("2nd block");
    },
    onLog3:function () {
        cc.log("3rd block");
    },
    onLog4:function () {
        cc.log("4th block");
    },
    title:function () {
        return "Issue 1305 #2";
    },
    subtitle:function () {
        return "See console. You should only see one message for each block";
    }
});

//------------------------------------------------------------------
//
// Issue1288
//
//------------------------------------------------------------------
var Issue1288 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(0);

        var spr = cc.Sprite.create(s_pathGrossini);
        spr.setPosition(100, 100);
        this.addChild(spr);

        var act1 = cc.MoveBy.create(0.5, cc.p(100, 0));
        var act2 = act1.reverse();
        var act3 = cc.Sequence.create(act1, act2);
        var act4 = cc.Repeat.create(act3, 2);

        spr.runAction(act4);
    },
    title:function () {
        return "Issue 1288";
    },
    subtitle:function () {
        return "Sprite should end at the position where it started.";
    }
});

//------------------------------------------------------------------
//
// Issue1288_2
//
//------------------------------------------------------------------
var Issue1288_2 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(0);

        var spr = cc.Sprite.create(s_pathGrossini);
        spr.setPosition(100, 100);
        this.addChild(spr);

        var act1 = cc.MoveBy.create(0.5, cc.p(100, 0));
        spr.runAction(cc.Repeat.create(act1, 1));
    },
    title:function () {
        return "Issue 1288 #2";
    },
    subtitle:function () {
        return "Sprite should move 100 pixels, and stay there";
    }
});

//------------------------------------------------------------------
//
// Issue1327
//
//------------------------------------------------------------------
var Issue1327 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(0);

        var spr = cc.Sprite.create(s_pathGrossini);
        spr.setPosition(100, 100);
        this.addChild(spr);

        var act1 = cc.CallFunc.create(this.onLogSprRotation);
        var act2 = cc.RotateBy.create(0.25, 45);
        var act3 = cc.CallFunc.create(this.onLogSprRotation, this);
        var act4 = cc.RotateBy.create(0.25, 45);
        var act5 = cc.CallFunc.create(this.onLogSprRotation.bind(this));
        var act6 = cc.RotateBy.create(0.25, 45);
        var act7 = cc.CallFunc.create(this.onLogSprRotation);
        var act8 = cc.RotateBy.create(0.25, 45);
        var act9 = cc.CallFunc.create(this.onLogSprRotation);

        var actF = cc.Sequence.create(act1, act2, act3, act4, act5, act6, act7, act8, act9);
        spr.runAction(actF);
    },
    onLogSprRotation:function (pSender) {
        cc.log(pSender.getRotation());
    },
    title:function () {
        return "Issue 1327";
    },
    subtitle:function () {
        return "See console: You should see: 0, 45, 90, 135, 180";
    }
});

//------------------------------------------------------------------
//
// Issue1438
//
//------------------------------------------------------------------
var Issue1438 = ActionsDemo.extend({
    onEnter:function () {
        this._super();
        this.centerSprites(2);

        //
        // manual animation
        //
        var animation = cc.Animation.create();

        // Add 60 frames
        for( var j=0; j<4; j++) {
            for (var i = 1; i < 15; i++) {
                var frameName = "res/Images/grossini_dance_" + ((i < 10) ? ("0" + i) : i) + ".png";
                animation.addSpriteFrameWithFile(frameName);
            }
        }
        // And display 60 frames per second
        animation.setDelayPerUnit(1/60);
        animation.setRestoreOriginalFrame(true);

        var action = cc.Animate.create(animation);
        this._kathia.runAction(action);

        //
        // File animation
        //
        var animCache = cc.AnimationCache.getInstance();
        animCache.addAnimations(s_animations2Plist);
        var animation2 = animCache.getAnimation("dance_1");
        animation2.setDelayPerUnit(1/60);

        var action2 = cc.Animate.create(animation2);
        this._tamara.runAction(cc.Sequence.create(action2, action2.reverse()));
    },

    title:function () {
        return "Animation";
    },

    subtitle:function () {
        return "Issue 1438. Set FPS to 30 to test this bug.";
    }
});
//-
//
// Flow control
//
var arrayOfActionsTest = [
    ActionManual,
    ActionMove,
    ActionScale,
    ActionRotate,
    ActionSkew,
    ActionSkewRotateScale,
    ActionJump,
    ActionBezier,
    Issue1008,
    ActionCardinalSpline,
    ActionCatmullRom,
    ActionBlink,
    ActionFade,
    ActionTint,
    ActionSequence,
    ActionSequence2,
    ActionSpawn,
    ActionReverse,
    ActionDelayTime,
    ActionRepeat,
    ActionRepeatForever,
    ActionRotateToRepeat,
    ActionRotateJerk,
    ActionCallFunc1,
    ActionCallFunc2,
    ActionCallFunc3,
    ActionReverseSequence,
    ActionReverseSequence2,
    ActionOrbit,
    ActionFollow,
    ActionTargeted,
    ActionTargetedCopy,
    PauseResumeActions,
    Issue1305,
    Issue1305_2,
    Issue1288,
    Issue1288_2,
    Issue1327,
    ActionAnimate,
    Issue1438
];

var nextActionsTest = function () {
    sceneIdx++;
    sceneIdx = sceneIdx % arrayOfActionsTest.length;

    return new arrayOfActionsTest[sceneIdx]();
};
var previousActionsTest = function () {
    sceneIdx--;
    if (sceneIdx < 0)
        sceneIdx += arrayOfActionsTest.length;

    return new arrayOfActionsTest[sceneIdx]();
};
var restartActionsTest = function () {
    return new arrayOfActionsTest[sceneIdx]();
};-}
