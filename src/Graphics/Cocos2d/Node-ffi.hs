{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Cocos2d (
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
  setScale,
  setVisible,
  setColor,
  setOpacity,
  setRatation
)
where

import Haste
import Haste.Prim
import Data.Functor
import Data.Word
import Graphics.Cocos2d.Utils

{- Cocos2d is a object oriented framework. To map this to haskell, a similar
 - approach as wxHaskell has been choosen.
 - You can read about it here: http://www.haskell.org/haskellwiki/WxHaskell/Quick_start
 - The basic Idea is this:
 - Node a is of class "a" which is derived from Node. So Node () is the Node class itself.
 - That way, anthing that derives from note can be passed where a Node is expected.
 -}

data CNode a
type Node a = Ptr (CNode a)

foreign import jscall "%1.addChild(%2,%3)"                addChild         :: Node a -> Node b -> Int -> IO ()
foreign import jscall "%1.addChild(%2)"                   addChild_        :: Node a -> Node b -> IO ()
foreign import jscall "%1.removeChild(%2)"                removeChild      :: Node a -> Node b -> IO ()
foreign import jscall "%1.setTag(%2)"                     setTag           :: Node a -> Int -> IO ()
foreign import jscall "%1.getChildByTag(%2)"              getChildByTag    :: Node a -> Int -> IO (Node ())
foreign import jscall "%1.getParent()"                    getParent        :: Node a -> IO (Node ())
foreign import jscall "%1.removeFromParent()"             removeFromParent :: Node a -> IO ()
foreign import jscall "%1.setAnchorPoint(%2)"             jsSetAnchorPoint :: Node a -> Point -> IO ()
setAnchorPoint :: Node a -> (Double, Double) -> IO ()
setAnchorPoint n p = tupleToPoint p >>= jsSetAnchorPoint n
foreign import jscall "%1.setPosition(%2)"                jsSetPosition    :: Node a -> Point -> IO ()
setPosition :: Node a -> (Double, Double) -> IO ()
setPosition n p = tupleToPoint p >>= jsSetPosition n
foreign import jscall "%1.getPosition()"                  jsGetPosition    :: Node a -> IO Point
getPosition :: Node a -> IO (Double, Double)
getPosition n = jsGetPosition n >>= pointToTouple
foreign import jscall "%1.setScale(%2,%3)"                setScale         :: Node a -> Double -> Double -> IO ()
foreign import jscall "%1.setVisible(%2)"                 setVisible       :: Node a -> Bool -> IO ()
foreign import jscall "%1.setColor(cc.c4b(%*))"           jsSetColor       :: Node a -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
setColor :: Node a -> Color4b -> IO ()
setColor n (Color4b r g b a) = jsSetColor n r g b a
foreign import jscall "%1.setOpacity(%2)"                 setOpacity       :: Node a -> Double -> IO ()
foreign import jscall "%1.setRotation(%2)"                setRotation      :: Node a -> Double -> IO ()
foreign import jscall "%1.getContentSize()"               jsGetContentSize :: Node a -> IO Size
getContentSize :: Node a -> IO (Double,Double)
getContentSize n = jsGetContentSize n >>= sizeToTuple
foreign import jscall "%1.setContentSize(cc.size(%2,%3))" setContentSize :: Node a -> Double -> Double -> IO ()
foreign import jscall "%1.schedule(function (a) {A(%2, [[1,a],0]);},%3)" scheduleInterval :: Node a -> IO () -> Double -> IO ()
foreign import jscall "%1.scheduleOnce(function (a) {A(%2, [[1,a],0]);},%3)" scheduleOnce :: Node a -> IO () -> Double -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().resumeTarget(%1)" resumeTarget :: Node a -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().pauseTarget(%1)"  pauseTarget :: Node a -> IO ()

-- array of nodes
nodeSetToNodeList :: NodeSet -> IO [Node ()]
nodeSetToNodeList set = do
  num <- nodeSetSize set
  mapM (\i -> nodeSetGetNthNode set i) [0..(num-1)]

foreign import jscall "%1.length" nodeSetSize    :: NodeSet -> IO Int
foreign import jscall "%1[%2]" nodeSetGetNthNode :: NodeSet -> Int -> IO Node

