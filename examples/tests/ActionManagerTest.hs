module ActionManagerTest where

data ActionManagerTest = AMT { prev :: ActionManagerTest,
                               this :: IO Scene,
                               next :: ActionManagerTest,
                               title :: String}

-- Generic creation of test scene
startSceneFromActionManagerTest :: ActionManagerTest -> IO ()
startSceneFromActionManagerTest amt = do
  (winWidth,winHeight) <- getWinSize
  scene <- (this amt)
  
  label <- createLabelTTF (title amt) "Arial" 32
  addChild scene label 1
  setPosition label (winWidth/2.0, winHeight -50.0)
  
  item1 <- createMenuItemImage s_pathB1 s_pathB2 (startSceneFromActionManagerTest (prev amt))
  item2 <- createMenuItemImage s_pathR1 s_pathR2 (startSceneFromActionManagerTest amt)
  item3 <- createMenuItemImage s_pathF1 s_pathF2 (startSceneFromActionManagerTest (next amt))

  menu <- createMenu [item1,item2,item3]

  setPosition menu (0.0,0.0)
  (width2,height2) <- getContentSize item2

  setPosition item1 (winWidth /2.0 - width2 *2.0, height2 /2.0)
  setPosition item2 (winWidth/2.0, height2 /2 .0)
  setPosition item3 (winWidth/2.0 + width2 * 2.0, height2 /2.0)
  
  addChild scene menu 1

  replaceScene scene

------------------------------------------------------------------
-- Test1
------------------------------------------------------------------
crashTest :: ActionManagerTest
crashTest = AMT resumeTest crashTestScene logicTest "Test 1. Should not crash"

crashTestScene :: IO Scene
crashTestScene = do
  scene <- createScene

  child <- createSprite s_pathGrossini
  setPostion child (200.0,200.0)
  addChild scene child 1
 
  -- Sum of all actions's duration is 1.5 second 
  runAction child (RotateBy 1.5 90.0)
  runAction child (Sequence [DelayTime 0.4, FadeOut 1.1])
  
  -- after 1.4 seconds, scene will be removed
  runAction scene (Sequence [DelayTime 1.4, CallFunc $ removeScene scene])

removeScene :: Scene -> IO ()
removeScene scene = do
  removeChild (getParent scene) scene
  onNextCallback this 
  


//------------------------------------------------------------------
//
// Test2
//
//------------------------------------------------------------------
var LogicTest = ActionManagerTest.extend({
    title:function () {
        return "Logic test";
    },
    onEnter:function () {
        this._super();

        var grossini = cc.Sprite.create(s_pathGrossini);
        this.addChild(grossini, 0, 2);
        grossini.setPosition(200, 200);

        grossini.runAction(cc.Sequence.create(
            cc.MoveBy.create(1, cc.p(150, 0)),
            cc.CallFunc.create(this.onBugMe, this))
        );
    },
    onBugMe:function (node) {
        node.stopAllActions(); //After this stop next action not working, if remove this stop everything is working
        node.runAction(cc.ScaleTo.create(2, 2));
    }
});

//------------------------------------------------------------------
//
// PauseTest
//
//------------------------------------------------------------------
var PauseTest = ActionManagerTest.extend({
    title:function () {
        return "Pause Test";
    },
    onEnter:function () {
        //
        // This test MUST be done in 'onEnter' and not on 'init'
        // otherwise the paused action will be resumed at 'onEnter' time
        //
        this._super();

        var s = director.getWinSize();
        var l = cc.LabelTTF.create("After 5 seconds grossini should move", "Thonburi", 16);
        this.addChild(l);
        l.setPosition(s.width / 2, 245);

        //
        // Also, this test MUST be done, after [super onEnter]
        //
        var grossini = cc.Sprite.create(s_pathGrossini);
        this.addChild(grossini, 0, TAG_GROSSINI);
        grossini.setPosition(200, 200);

        var action = cc.MoveBy.create(1, cc.p(150, 0));

        director.getActionManager().addAction(action, grossini, true);

        this.schedule(this.onUnpause, 3);
    },
    onUnpause:function (dt) {
        this.unschedule(this.onUnpause);
        var node = this.getChildByTag(TAG_GROSSINI);
        director.getActionManager().resumeTarget(node);
    }
});

//------------------------------------------------------------------
//
// RemoveTest
//
//------------------------------------------------------------------
var RemoveTest = ActionManagerTest.extend({
    title:function () {
        return "Remove Test";
    },
    onEnter:function () {
        this._super();

        var s = director.getWinSize();
        var l = cc.LabelTTF.create("Should not crash", "Thonburi", 16);
        this.addChild(l);
        l.setPosition(s.width / 2, 245);

        var move = cc.MoveBy.create(2, cc.p(200, 0));
        var callback = cc.CallFunc.create(this.stopAction, this);
        var sequence = cc.Sequence.create(move, callback);
        sequence.setTag(TAG_SEQUENCE);

        var child = cc.Sprite.create(s_pathGrossini);
        child.setPosition(200, 200);

        this.addChild(child, 1, TAG_GROSSINI);
        child.runAction(sequence);
    },
    stopAction:function () {
        var sprite = this.getChildByTag(TAG_GROSSINI);
        sprite.stopActionByTag(TAG_SEQUENCE);
    }
});

//------------------------------------------------------------------
//
// ResumeTest
//
//------------------------------------------------------------------
var ResumeTest = ActionManagerTest.extend({
    title:function () {
        return "Resume Test";
    },
    onEnter:function () {
        this._super();

        var s = director.getWinSize();
        var l = cc.LabelTTF.create("Grossini only rotate/scale in 3 seconds", "Thonburi", 16);
        this.addChild(l);
        l.setPosition(s.width / 2, 245);

        var grossini = cc.Sprite.create(s_pathGrossini);
        this.addChild(grossini, 0, TAG_GROSSINI);
        grossini.setPosition(s.width / 2, s.height / 2);

        grossini.runAction(cc.ScaleBy.create(2, 2));

        director.getActionManager().pauseTarget(grossini);
        grossini.runAction(cc.RotateBy.create(2, 360));

        this.schedule(this.resumeGrossini, 3.0);
    },
    resumeGrossini:function (time) {
        this.unschedule(this.resumeGrossini);

        var grossini = this.getChildByTag(TAG_GROSSINI);
        director.getActionManager().resumeTarget(grossini);
    }
});

var ActionManagerTestScene = TestScene.extend({
    runThisTest:function () {
        sceneIdx = -1;
        MAX_LAYER = 5;
        var layer = nextActionManagerAction();
        this.addChild(layer);
        director.replaceScene(this);
    }
});

