module Graphics.Cocos2d.Scene where

import Graphics.Cocos2d
import Foreign.Ptr
import Haste
import Haste.Prim

data SceneType
type Scene = Ptr SceneType

-- Construtor
foreign import jscall "createHSScene(%1)" createScene :: (Scene -> IO ()) -> IO Scene
createScene_ = createScene (\_ -> return ())

-- set a scene as active scene
foreign import jscall "cc.Director.getInstance().runWithScene(%1)" runWithScene :: Scene -> IO ()
foreign import jscall "cc.Director.getInstance().replaceScene(%1)" replaceScene :: Scene -> IO ()

-- Transitions
foreign import jscall "cc.TransitionProgressRadialCCW.create(%1, %2)" createTransitionProgressRadialCCW :: Double -> Scene -> IO Scene

instance NodeDerived Scene where
  toNode = sceneToNode
foreign import ccall "returnSame" sceneToNode :: Scene -> Node
