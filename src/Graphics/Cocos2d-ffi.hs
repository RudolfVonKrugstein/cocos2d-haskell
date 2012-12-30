{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Cocos2d where

import Haste
import Haste.Prim
import Data.Functor
import Data.Word
import Graphics.Cocos2d.Action

data AppType
type App = Ptr AppType
--data DirectorType
--type Director = Ptr DirectorType
data LayerType
type Layer = Ptr LayerType
data SizeType
type Size = Ptr SizeType
data PointType
type Point = Ptr PointType
data MenuItemImageType
type MenuItemImage = Ptr MenuItemImageType
data MenuItemLabelType
type MenuItemLabel = Ptr MenuItemLabelType
data MenuType
type Menu = Ptr MenuType
data SpriteType
type Sprite = Ptr SpriteType
data LabelTTFType
type LabelTTF = Ptr LabelTTFType

--base classes
data NodeType
type Node = Ptr NodeType
data MenuItemType
type MenuItem = Ptr MenuItemType

data Color4b = Color4b Word8 Word8 Word8 Word8

-- Start cococs2d app
foreign import jscall "startCocos2dApp(function (a) {A(%1, [[1,a],0]);})" cocos2dApp :: (App -> IO ()) -> IO ()

--foreign import jscall "cc.Director.getInstance()" getDirectorInstance :: IO Director

foreign import jscall "cc.Director.getInstance().setDisplayStats(%1)" setDisplayStats :: Bool -> IO ()

foreign import jscall "cc.Director.getInstance().setAnimationInterval(%1)" setAnimationInterval :: Double -> IO ()

foreign import jscall "new cc.Layer()" createLayer :: IO Layer

foreign import jscall "cc.LayerGradient.create(cc.c4b(%1,%2,%3,%4), cc.c4b(%5,%6,%7,%8))" createLayerGradientJS :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO Layer
foreign import jscall "cc.LayerColor.create(cc.c4b(%1,%2,%3,%4))" createLayerColorJS :: Word8 -> Word8 -> Word8 -> Word8 -> IO Layer
createLayerGradient (Color4b a1 a2 a3 a4) (Color4b b1 b2 b3 b4) = createLayerGradientJS a1 a2 a3 a4 b1 b2 b3 b4
createLayerColor (Color4b r g b a) = createLayerColorJS r g b a

foreign import jscall "%1.width" sizeWidth :: Size -> Double
foreign import jscall "%1.height" sizeHeight :: Size -> Double

sizeToTuple :: Size -> (Double,Double)
sizeToTuple s = (sizeWidth s, sizeHeight s)

foreign import jscall "%1.x" pointX :: Point -> Double
foreign import jscall "%1.y" pointY :: Point -> Double
foreign import jscall "cc.p(%1,%2)" point :: Double -> Double -> Point
tupleToPoint :: (Double,Double) -> Point
tupleToPoint t = point (fst t) (snd t)
pointToTuple :: Point -> (Double,Double)
pointToTuple p = (pointX p, pointY p)

foreign import jscall "cc.PointZero()" pointZeroJS :: IO Point
pointZero :: IO (Double,Double)
pointZero = pointToTuple <$> pointZeroJS


foreign import jscall "cc.Director.getInstance().getWinSize()" getWinSizeJS :: IO Size

getWinSize :: IO (Double,Double)
getWinSize = sizeToTuple <$> getWinSizeJS

foreign import jscall "cc.MenuItemImage.create(%1,%2,%3,0)" createMenuItemImage :: String -> String -> IO () -> IO MenuItemImage
foreign import jscall "cc.MenuItemLabel.create(%1,%2,0)" createMenuItemLabel :: LabelTTF -> IO () -> IO MenuItemLabel

foreign import jscall "history.go(-1)" quit :: IO ()

-- type class to controll classes derived von CCNode
class NodeDerived a where
  toNode :: a -> Node

  addChild :: (NodeDerived b) => a -> b -> Int -> IO ()
  addChild a b id = nodeAddChild (toNode a) (toNode b) id

  addChild_ :: (NodeDerived b) => a -> b -> IO ()
  addChild_ a b = nodeAddChild_ (toNode a) (toNode b)

  setTag :: a -> Int -> IO ()
  setTag a = nodeSetTag (toNode a)

  getChildByTag :: a -> Int -> IO Node
  getChildByTag a id = nodeGetChildByTag (toNode a) id

  getParent :: a -> IO Node
  getParent a = nodeGetParent (toNode a)

  removeChild :: (NodeDerived b) => a -> b -> IO ()
  removeChild a b = nodeRemoveChild (toNode a) (toNode b)

  setAnchorPoint :: a -> (Double,Double) -> IO ()
  setAnchorPoint a p = nodeSetAnchorPoint (toNode a) (tupleToPoint p)

  setPosition :: a -> (Double,Double) -> IO ()
  setPosition a p = nodeSetPosition (toNode a) (tupleToPoint p)

  getPosition :: a -> IO (Double,Double)
  getPosition a = pointToTuple <$> nodeGetPosition (toNode a)

  setScale :: a -> (Double,Double) -> IO ()
  setScale a (x,y) = nodeSetScale (toNode a) x y

  setVisible :: a -> Bool -> IO ()
  setVisible a t = nodeSetVisible (toNode a) t

  setColor :: a -> Color4b -> IO ()
  setColor a (Color4b r g b al) = nodeSetColor (toNode a) r g b al

  setOpacity :: a -> Double -> IO ()
  setOpacity a o = nodeSetOpacity (toNode a) o

  setRotation :: a -> Double -> IO ()
  setRotation a = nodeSetRotation (toNode a)

  getContentSize :: a -> IO (Double,Double)
  getContentSize a = sizeToTuple <$> nodeGetContentSize (toNode a)

  setContentSize :: a -> (Double,Double) -> IO ()
  setContentSize a (w,h) = nodeSetContentSize (toNode a) w h

  addAction :: Action -> a -> Bool -> IO ()
  addAction a n b = do
    a1 <- toJSAction a
    nodeAddAction a1 (toNode n) b
  
  runAction :: a -> Action -> IO ()
  runAction n a = do
    a1 <- toJSAction a
    nodeRunAction (toNode n) $! a1

  stopAllActions :: a -> IO ()
  stopAllActions a = nodeStopAllActions (toNode a)

  stopActionByTag :: a -> Int -> IO ()
  stopActionByTag a i = nodeStopActionByTag (toNode a) i

  scheduleInterval :: a -> IO () -> Double -> IO ()
  scheduleInterval a f i = nodeScheduleInterval (toNode a) f i

  scheduleOnce :: a -> IO () -> Double -> IO ()
  scheduleOnce a f d = nodeScheduleOnce (toNode a) f d

  resumeTarget :: a -> IO ()
  resumeTarget a = nodeResumeTarget (toNode a)

  pauseTarget :: a -> IO ()
  pauseTarget a = nodePauseTarget (toNode a)

foreign import jscall "%1.addChild(%2,%3)" nodeAddChild :: Node -> Node -> Int -> IO ()
foreign import jscall "%1.addChild(%2)" nodeAddChild_ :: Node -> Node -> IO ()
foreign import jscall "%1.removeChild(%2)" nodeRemoveChild :: Node -> Node -> IO ()
foreign import jscall "%1.setTag(%2)" nodeSetTag :: Node -> Int -> IO ()
foreign import jscall "%1.getChildByTag(%2)" nodeGetChildByTag :: Node -> Int -> IO Node
foreign import jscall "%1.getParent()" nodeGetParent :: Node -> IO Node
foreign import jscall "%1.setAnchorPoint(%2)" nodeSetAnchorPoint :: Node -> Point -> IO ()
foreign import jscall "%1.setPosition(%2)" nodeSetPosition :: Node -> Point -> IO ()
foreign import jscall "%1.getPosition()" nodeGetPosition :: Node -> IO Point
foreign import jscall "%1.setScale(%2,%3)" nodeSetScale :: Node -> Double -> Double -> IO ()
foreign import jscall "%1.setVisible(%2)" nodeSetVisible :: Node -> Bool -> IO ()
foreign import jscall "%1.setColor(cc.c4b(%*))" nodeSetColor :: Node -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
foreign import jscall "%1.setOpacity(%2)" nodeSetOpacity :: Node -> Double -> IO ()
foreign import jscall "%1.setRotation(%2)" nodeSetRotation :: Node -> Double -> IO ()
foreign import jscall "%1.getContentSize()" nodeGetContentSize :: Node -> IO Size
foreign import jscall "%1.setContentSize(cc.s(%2,%3))" nodeSetContentSize :: Node -> Double -> Double -> IO ()
foreign import jscall "%1.runAction(%2)" nodeRunAction :: Node -> JSAction -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().addAction(%1,%2,%3)" nodeAddAction :: JSAction -> Node -> Bool -> IO ()
foreign import jscall "%1.stopAllActions()" nodeStopAllActions :: Node -> IO ()
foreign import jscall "%1.stopActionByTag(%2)" nodeStopActionByTag :: Node -> Int -> IO ()
foreign import jscall "%1.schedule(function (a) {A(%2, [[1,a],0]);},%3)" nodeScheduleInterval :: Node -> IO () -> Double -> IO ()
foreign import jscall "%1.scheduleOnce(function (a) {A(%2, [[1,a],0]);},%3)" nodeScheduleOnce :: Node -> IO () -> Double -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().resumeTarget(%1)" nodeResumeTarget :: Node -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().pauseTarget(%1)" nodePauseTarget :: Node -> IO ()

-- instances
instance NodeDerived Node where
  toNode = id

instance NodeDerived Sprite where
  toNode = spriteToNode
foreign import ccall "returnSame" spriteToNode :: Sprite -> Node

instance NodeDerived Layer where
  toNode = layerToNode
foreign import ccall "returnSame" layerToNode :: Layer -> Node

instance NodeDerived LabelTTF where
  toNode = labelttfToNode
foreign import ccall "returnSame" labelttfToNode :: LabelTTF -> Node

instance NodeDerived MenuItemImage where
  toNode = menuItemImageToNode
foreign import ccall "returnSame" menuItemImageToNode :: MenuItemImage -> Node

instance NodeDerived MenuItemLabel where
  toNode = menuItemLabelToNode
foreign import ccall "returnSame" menuItemLabelToNode :: MenuItemLabel -> Node

instance NodeDerived Menu where
  toNode = menuToNode
foreign import ccall "returnSame" menuToNode :: Menu -> Node

foreign import jscall "cc.Menu.create()" createMenu :: IO Menu
createMenuWithItems :: [MenuItem] -> IO Menu
createMenuWithItems items = do
  m <- createMenu
  mapM_ (addMenuItem m) items
  return m

foreign import jscall "%1.addChild(%2)" menuAddMenuItem :: Menu -> MenuItem -> IO ()

foreign import jscall "cc.LabelTTF.create(%1,%2,%3)" createLabelTTF :: String -> String -> Int -> IO LabelTTF

foreign import jscall "cc.Sprite.create(%1)" createSprite :: String -> IO Sprite

class MenuItemDerived i where
  toMenuItem :: i -> MenuItem
  addMenuItem :: Menu -> i -> IO ()
  addMenuItem m i = menuAddMenuItem m (toMenuItem i)

instance MenuItemDerived MenuItem where
  toMenuItem = id

instance MenuItemDerived MenuItemImage where
  toMenuItem = menuItemImageToMenuItem
foreign import ccall "returnSame" menuItemImageToMenuItem :: MenuItemImage -> MenuItem

instance MenuItemDerived MenuItemLabel where
  toMenuItem = menuItemLabelToMenuItem
foreign import ccall "returnSame" menuItemLabelToMenuItem :: MenuItemLabel -> MenuItem

