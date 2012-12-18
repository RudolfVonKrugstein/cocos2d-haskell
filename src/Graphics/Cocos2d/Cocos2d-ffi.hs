module Cocos2d where

type App = JSAny
type Director = JSAny
type Scene = JSAny
type Layer = JSAny
type Size = JSAny
type Point = JSAny
type Menuitem = JSAny
type Menu = JSAny
type Sprite = JSAny
type TTFLabel = JSAny

-- Start cococs2d app
foreign import jscall "myApp = cocos2dApp(function (a) {A(%1, [[1,a],0]);})" cocos2dApp :: (App -> IO ()) -> IO ()

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

foreign import jscall "cc.p(%1,%2)" point :: Double -> Double -> Point
tupleToPoint :: (Double,Double) -> Point
tupleToPoint t = point (fst t) (snd t)


foreign import jscall "cc.Director.getInstance().getWinSize()" getWinSizeJS :: IO Size

getWinSize :: IO (Double,Double)
getWinSize = sizeToTuple <$> getWinSizeJS

foreign import jscall "cc.MenuItemImage.create(%1,%2,%3,NULL)" createMenuItemImage :: String -> String -> IO () -> IO MenuItem

foreign import jscall "history.go(-1)" quit :: IO ()

-- type class to controll what can be addes as child
class AddChild a b
instance AddChild Scene Layer
instance AddChild Menu MenuItem
instance AddChild Layer TTFLabel

foreign import jscall "%1.addChild(%2)" addChild :: (AddChild a b) => a -> b -> IO ()

-- type class for everything that can be transformed
class Tranformable a
instance Transformable MenuItem
instance Transformable Menu
instance Transformable Sprite
instance Transformable TTFLabel

foreign import jscall "%1.setAnchorPoint(%2)" setAnchorPointJS :: (Tranformable a) => a -> Point -> IO ()
setAnchorPoint :: (Tranformable a) => a -> (Double,Double) -> IO ()
setAnchorPoint t p = setAnchorPointJS t (tupleToPoint p)

foreign import jscall "%1.setPosition(%2)" setPositionJS :: (Tranformable a) => a -> Point -> IO ()
setPosition :: (Tranformable a) => a -> (Double,Double) -> IO ()
setPosition t p = setPositionJS t (tupleToPoint p)

foreign import jscall "%1.setScale(%2)" setScale :: (Transformable a) => a -> Double -> IO ()
foreign import jscall "%1.setRotation(%2)" setRotation :: (Transformable a) => a -> Double -> IO ()

foreign import jscall "cc.Menu.create()" createMenu :: IO Menu
createMenuWithItems :: [MenuItem] -> IO Menu
createMenuWithItems items = do
  m <- createMenu
  mapM_ (addChild m) items
  return m

foreign import jscall "cc.LabelTTF.create(%1,%2,%3)" createLabelTTF :: String -> String -> Int -> IO TTFLabel

foreign import jscall "cc.Sprite.create(%1)" createSprite :: String -> IO Sprite

-- Actions
data Action = Sequence [Action] | RotateTo Double Double | ScaleTo Double (Double,Double) | MoveBy Double (Double,Double)
data JSAction = JSAny

toJSAction :: Action -> JSAction
toJSAction (Sequence a:as)   = sequenceAction (toJSAction a) (toJSAction $ Sequence as)
toJSAction (Sequence [a])    = toJSAction a
toJSAction (RotateTo t r)    = rotateToAction t r
toJSAction (ScaleTo t (x,y)) = scaleToAction t x y
toJSAction (MoveBy t (x,y))  = moveByAction t x y

foreign import jscall "cc.Sequence.create(%1,%2)" sequenceAction :: JSAction -> JSAction -> JSAction
foreign import jscall "cc.RotateTo(%1,%2)" rotateToAction :: Double -> Double -> JSAction
foreign import jscall "cc.ScaleTo(%1,%2,%3)" scaleToAction :: Double -> Double -> Double -> JSAction
foreign import jscall "cc.MoveBy.create(%1,cc.p(%2,%3))" moveByAction :: Double -> Double -> Double -> JSAction

foreign import "%1.runAction(%2)" runActionJS :: (Transformable a) => a -> JSAction -> IO ()
runAction :: (Transformable a) => a -> Action -> IO ()
runAction t a = runActionJS t (toJSAction a)
