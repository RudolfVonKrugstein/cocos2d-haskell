module Graphics.Cocos2d.Scene (
  Scene,
  createScene,
  createScene_,
  runWithScene,
  replaceScene,
  createTransitionProgressRadialCCW
) where

import Graphics.Cocos2d.Utils
import Graphics.Cocos2d.Node
import Foreign.Ptr
import Haste
import Haste.Prim

{- Scene is derived from node, every action on node can also be done on scene
 -}

data CScene a
type Scene a = Node (CScene a)

-- Construtor
foreign import jscall "new HaskellScene(function(s) {A(%1,[[1,s],0]);})" createScene :: ((Scene ()) -> IO ()) -> IO	(Scene ())
createScene_ = createScene (\_ -> return ())

-- set a scene as active scene
foreign import jscall "cc.Director.getInstance().runWithScene(%1)" runWithScene :: Scene a -> IO ()
foreign import jscall "cc.Director.getInstance().replaceScene(%1)" replaceScene :: Scene a -> IO ()

-- Transitions
foreign import jscall "cc.TransitionProgressRadialCCW.create(%1, %2)" createTransitionProgressRadialCCW :: Double -> Scene a -> IO (Scene ())

