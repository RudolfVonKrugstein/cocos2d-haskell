module Graphics.Cocos2d.Scene where

import Graphics.Cocos2d
import Foreign.Ptr

data SceneType
type Scene = Ptr SceneType

-- Construtor
foreign import jscall "new cc.Scene()" createScene :: IO Scene

-- set a scene as active scene
foreign import jscall "cc.Director.getInstance().runWithScene(%1)" runWithScene :: Scene -> IO ()
foreign import jscall "cc.Director.getInstance().replaceScene(%1)" replaceScene :: Scene -> IO ()

-- Transitions
foreign import jscall "cc.TransitionProgressRadialCCW.create(%1, %2)" createTransitionProgressRadialCCW :: Double -> Scene -> IO Scene

instance NodeDerived Scene where
  toNode = sceneToNode
foreign import ccall "returnSame" sceneToNode :: Scene -> Node
