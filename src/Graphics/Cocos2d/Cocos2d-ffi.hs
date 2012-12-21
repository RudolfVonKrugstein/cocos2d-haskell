module Cocos2d where

import Haste
import Haste.Prim
import Data.Functor

data AppType
type App = Ptr AppType
data DirectorType
type Director = Ptr DirectorType
data SceneType
type Scene = Ptr SceneType
data LayerType
type Layer = Ptr LayerType
data SizeType
type Size = Ptr SizeType
data PointType
type Point = Ptr PointType
data MenuItemType
type MenuItem = Ptr MenuItemType
data MenuType
type Menu = Ptr MenuType
data SpriteType
type Sprite = Ptr SpriteType
data TTFLabelType
type TTFLabel = Ptr TTFLabelType
data NodeType
type Node = Ptr NodeType

-- Start cococs2d app
foreign import jscall "startCocos2dApp(function (a) {A(%1, [[1,a],0]);})" cocos2dApp :: (App -> IO ()) -> IO ()

foreign import jscall "cc.Director.getInstance()" getDirectorInstance :: IO Director

foreign import jscall "%1.setDisplayStats(%2)" setDisplayStats :: Director -> Bool -> IO ()

foreign import jscall "%1.setAnimationInterval(%2)" setAnimationInterval :: Director -> Double -> IO ()

foreign import jscall "%1.runWithScene(%2)" runWithScene :: Director -> Scene -> IO ()

foreign import jscall "new cc.Scene()" createScene :: IO Scene

foreign import jscall "new cc.Layer()" createLayer :: IO Layer

foreign import jscall "%1.width" sizeWidth :: Size -> Double
foreign import jscall "%1.height" sizeHeight :: Size -> Double

sizeToTuple :: Size -> (Double,Double)
sizeToTuple s = (sizeWidth s, sizeHeight s)

foreign import jscall "%1.x" pointX :: Point -> Double
foreign import jscall "%1.z" pointY :: Point -> Double
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

foreign import jscall "cc.MenuItemImage.create(%1,%2,%3,0)" createMenuItemImage :: String -> String -> IO () -> IO MenuItem

foreign import jscall "history.go(-1)" quit :: IO ()

-- type class to controll classes derived von CCNode
class NodeDerived a where
  toNode :: a -> Node

  addChild :: (NodeDerived b) => a -> b -> IO ()
  addChild a b = nodeAddChild (toNode a) (toNode b)

  setAnchorPoint :: a -> (Double,Double) -> IO ()
  setAnchorPoint a p = nodeSetAnchorPoint (toNode a) (tupleToPoint p)

  setPosition :: a -> (Double,Double) -> IO ()
  setPosition a p = nodeSetPosition (toNode a) (tupleToPoint p)

  setScale :: a -> Double -> IO ()
  setScale a = nodeSetScale (toNode a)
  
  setRotation :: a -> Double -> IO ()
  setRotation a = nodeSetRotation (toNode a)
  
  runAction :: a -> Action -> IO ()
  runAction n a = nodeRunAction (toNode n) $! (toJSAction a)

foreign import jscall "%1.addChild(%2)" nodeAddChild :: Node -> Node -> IO ()
foreign import jscall "%1.setAnchorPoint(%2)" nodeSetAnchorPoint :: Node -> Point -> IO ()
foreign import jscall "%1.setPosition(%2)" nodeSetPosition :: Node -> Point -> IO ()
foreign import jscall "%1.setScale(%2)" nodeSetScale :: Node -> Double -> IO ()
foreign import jscall "%1.setRotation(%2)" nodeSetRotation :: Node -> Double -> IO ()
foreign import jscall "%1.runAction(%2)" nodeRunAction :: Node -> JSAction -> IO ()

-- instances
instance NodeDerived MenuItem where
  toNode = menuItemToNode
foreign import jscall "%1" menuItemToNode :: MenuItem -> Node

instance NodeDerived Menu where
  toNode = menuToNode
foreign import jscall "%1" menuToNode :: Menu -> Node

instance NodeDerived Sprite where
  toNode = spriteToNode
foreign import jscall "%1" spriteToNode :: Sprite -> Node

instance NodeDerived Layer where
  toNode = layerToNode
foreign import jscall "%1" layerToNode :: Layer -> Node

instance NodeDerived TTFLabel where
  toNode = ttflabelToNode
foreign import jscall "%1" ttflabelToNode :: TTFLabel -> Node

instance NodeDerived Scene where
  toNode = sceneToNode
foreign import jscall "%1" sceneToNode :: Scene -> Node

foreign import jscall "cc.Menu.create()" createMenu :: IO Menu
createMenuWithItems :: [MenuItem] -> IO Menu
createMenuWithItems items = do
  m <- createMenu
  mapM_ (addChild m) items
  return m

foreign import jscall "cc.LabelTTF.create(%1,%2,%3)" createLabelTTF :: String -> String -> Int -> IO TTFLabel

foreign import jscall "cc.Sprite.create(%1)" createSprite :: String -> IO Sprite

-- Actions
data Action = Sequence [Action] | RotateTo Double Double | ScaleTo Double (Double,Double) | MoveBy Double (Double,Double) deriving (Show)
data JSActionType
type JSAction = Ptr JSActionType

toJSAction :: Action -> JSAction
{-toJSAction a = unsafePerformIO $ do
  alert (show a)
  return -}
toJSAction (Sequence (a:b:as))   = sequenceAction (toJSAction a) (toJSAction $ Sequence (b:as))
toJSAction (Sequence [a])    = toJSAction a
toJSAction (RotateTo t r)    = rotateToAction t r
toJSAction (ScaleTo t (x,y)) = scaleToAction t x y
toJSAction (MoveBy t (x,y))  = moveByAction t x y

foreign import jscall "cc.Sequence.create(%1,%2)" sequenceAction :: JSAction -> JSAction -> JSAction
foreign import jscall "cc.RotateTo.create(%1,%2)" rotateToAction :: Double -> Double -> JSAction
foreign import jscall "cc.ScaleTo.create(%1,%2,%3)" scaleToAction :: Double -> Double -> Double -> JSAction
foreign import jscall "cc.MoveBy.create(%1,cc.p(%2,%3))" moveByAction :: Double -> Double -> Double -> JSAction

