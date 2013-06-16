module Graphics.Cocos2d.Node (
  Node,
  NodeSet,
  NodeBase (..),
  addChild,
  addChild_,
  removeChild,
  setTag,
  getChildByTag,
  getParent,
  removeFromParent,
  setAnchorPoint,
  setPosition,
  getPosition,
  setZOrder,
  getZOrder,
  setScale,
  setVisible,
  setColor,
  setOpacity,
  setRotation,
  setContentSize,
  getContentSize,
  nodeSetToNodeList,
  scheduleInterval,
  scheduleOnce,
  resumeTarget,
  resumeTargets,
  pauseTarget
)
where

import Haste
import Haste.Prim
import Data.Functor
import Data.Word
import Graphics.Cocos2d.Utils

{- Cocos2d is a object oriented framework. To map this to haskell, type classes for every class
 - in cocos2d are introduced. A derived class is part of the type class of its base class.
 - A function "to<Base>" converts the derived class to the base class before calling a function of the base class.
 - The haste-ffi-parser takes care of creating the appropriate functions, that converts a function taking a base class into
 - a function, taking a member of the appropriate type class, and calling the convert function.
 -}

data CNode
type Node = Ptr CNode
data CNodeSet
type NodeSet = Ptr CNodeSet

class NodeBase a where
  toNode :: a -> Node

instance NodeBase Node where
  toNode = id

foreign import cpattern "%1.addChild(%2,%3)"                addChild         :: (NodeBase a, NodeBase b) => a -> b -> Int -> IO ()
foreign import cpattern "%1.addChild(%2)"                   addChild_        :: (NodeBase a, NodeBase b) => a -> b -> IO ()
foreign import cpattern "%1.removeChild(%2)"                removeChild      :: (NodeBase a, NodeBase b) => a -> b -> IO ()
foreign import cpattern "%1.setTag(%2)"                     setTag           :: (NodeBase a) => a -> Int -> IO ()
foreign import cpattern "%1.getChildByTag(%2)"              getChildByTag    :: (NodeBase a) => a -> Int -> IO Node
foreign import cpattern "%1.getParent()"                    getParent        :: (NodeBase a) => a -> IO Node
foreign import cpattern "%1.removeFromParent()"             removeFromParent :: (NodeBase a) => a -> IO ()
foreign import cpattern "%1.setAnchorPoint(%2)"             jsSetAnchorPoint :: (NodeBase a) => a -> Point -> IO ()
setAnchorPoint :: (NodeBase a) => a -> (Double, Double) -> IO ()
setAnchorPoint n p = tupleToPoint p >>= jsSetAnchorPoint n
foreign import cpattern "%1.setPosition(%2)"                jsSetPosition    :: (NodeBase a) => a -> Point -> IO ()
setPosition :: (NodeBase a) => a -> (Double, Double) -> IO ()
setPosition n p = tupleToPoint p >>= jsSetPosition n
foreign import cpattern "%1.getPosition()"                  jsGetPosition    :: (NodeBase a ) => a -> IO Point
getPosition :: NodeBase a => a -> IO (Double, Double)
getPosition n = jsGetPosition n >>= pointToTuple
foreign import cpattern "%1.setZOrder(%2)"                  setZOrder        :: (NodeBase a) => a -> Int -> IO ()
foreign import cpattern "%1.getZOrder()"                    getZOrder        :: (NodeBase a) => a -> IO Int
foreign import cpattern "%1.setScale(%2,%3)"                setScale         :: (NodeBase a) => a -> Double -> Double -> IO ()
foreign import cpattern "%1.setVisible(%2)"                 setVisible       :: (NodeBase a) => a -> Bool -> IO ()
foreign import cpattern "%1.setColor(cc.c4b(%*))"           jsSetColor       :: (NodeBase a) => a -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
setColor :: NodeBase a => a -> Color4b -> IO ()
setColor n (Color4b r g b a) = jsSetColor n r g b a
foreign import cpattern "%1.setOpacity(%2)"                 setOpacity       :: (NodeBase a) => a -> Double -> IO ()
foreign import cpattern "%1.setRotation(%2)"                setRotation      :: (NodeBase a) => a -> Double -> IO ()
foreign import cpattern "%1.getContentSize()"               jsGetContentSize :: (NodeBase a) => a -> IO Size
getContentSize :: NodeBase a => a -> IO (Double,Double)
getContentSize n = jsGetContentSize n >>= sizeToTuple
foreign import cpattern "%1.setContentSize(cc.size(%2,%3))" setContentSize :: (NodeBase a) => a -> Double -> Double -> IO ()
foreign import cpattern "%1.schedule(function (a) {A(%2, [[1,a],0]);},%3)" scheduleInterval :: (NodeBase a) => a -> IO () -> Double -> IO ()
foreign import cpattern "%1.scheduleOnce(function (a) {A(%2, [[1,a],0]);},%3)" scheduleOnce :: (NodeBase a) => a -> IO () -> Double -> IO ()
foreign import cpattern "cc.Director.getInstance().getActionManager().resumeTarget(%1)" resumeTarget :: (NodeBase a) => a -> IO ()
resumeTargets :: [Node] -> IO ()
resumeTargets = mapM_ (resumeTarget)
foreign import cpattern "cc.Director.getInstance().getActionManager().pauseTarget(%1)"  pauseTarget  :: (NodeBase a) => a -> IO ()

-- array of nodes
nodeSetToNodeList :: NodeSet -> IO [Node]
nodeSetToNodeList set = do
  num <- nodeSetSize set
  mapM (\i -> nodeSetGetNthNode set i) [0..(num-1)]

foreign import cpattern "%1.length" nodeSetSize    :: NodeSet -> IO Int
foreign import cpattern "%1[%2]" nodeSetGetNthNode :: NodeSet -> Int -> IO (Node)

