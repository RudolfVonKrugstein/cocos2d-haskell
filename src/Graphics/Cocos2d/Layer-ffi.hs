module Graphics.Cocos2d.Layer where

import Haste
import Haste.Prim
import Data.Functor
import Control.Applicative
import Data.VectorSpace
import Data.Word

import Graphics.Cocos2d.Utils
import Graphics.Cocos2d.Node
import Foreign.Ptr

data CLayer
type Layer = Ptr CLayer

class LayerBase a where
  toLayer :: a -> Layer

instance LayerBase Layer where
  toLayer = id
instance NodeBase Layer where
  toNode = layerToNode
foreign import ccall "returnSame" layerToNode :: Layer -> Node

foreign import cpattern "new cc.Layer()" createLayer :: IO Layer
foreign import cpattern "cc.LayerGradient.create(cc.c4b(%1,%2,%3,%4), cc.c4b(%5,%6,%7,%8))" jsCreateLayerGradient :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO Layer
foreign import cpattern "cc.LayerColor.create(cc.c4b(%1,%2,%3,%4))" jsCreateLayerColor :: Word8 -> Word8 -> Word8 -> Word8 -> IO Layer
createLayerGradient (Color4b a1 a2 a3 a4) (Color4b b1 b2 b3 b4) = jsCreateLayerGradient a1 a2 a3 a4 b1 b2 b3 b4
createLayerColor (Color4b r g b a) = jsCreateLayerColor r g b a

{- All touch functionality is directly in the layer
 - because CCLayer is the only class we need that
 - derives from TouchDelegate and we can not encode
 - multiple inherintance.
 - Hopefully that will not make problems in the fututure ...
 -}
data Touch = Touch {
               touchLocation :: (Double,Double),
               touchPrevLocation :: (Double,Double)
             }
touchDelta :: Touch -> (Double,Double)
touchDelta (Touch l pl) = l ^-^ pl

-- Foreign type
data CTouch
data CTouchSet

-- convert foreign touch
foreign import cpattern "%1.getLocation()"          c_touchGetLocation         :: Ptr CTouch -> IO Point
foreign import cpattern "%1.getPreviousLocation()"  c_touchGetPreviousLocation :: Ptr CTouch -> IO Point

c_touchToTouch :: Ptr CTouch -> IO Touch
c_touchToTouch ft = Touch <$> (c_touchGetLocation ft >>= pointToTuple)
                          <*> (c_touchGetPreviousLocation ft >>= pointToTuple)

c_touchesToTouchList :: Ptr CTouchSet -> IO [Touch]
c_touchesToTouchList set = do
  num <- c_touchSetGetNumTouches set
  mapM (\i -> c_touchSetGetNthTouch set i >>= c_touchToTouch) [0..(num-1)]

foreign import cpattern "%1.length" c_touchSetGetNumTouches :: Ptr CTouchSet -> IO Int
foreign import cpattern "%1[%2]"    c_touchSetGetNthTouch   :: Ptr CTouchSet -> Int -> IO (Ptr CTouch)

setOnTouchBegan :: (LayerBase a) => a -> (Touch -> IO Bool) -> IO ()
setOnTouchBegan a f =
  c_setOnTouchBegan a (\i -> c_touchToTouch i >>= f)

setOnTouchMoved :: (LayerBase a) => a -> (Touch -> IO ()) -> IO ()
setOnTouchMoved a f =
  c_setOnTouchMoved a (\i -> c_touchToTouch i >>= f)

setOnTouchEnded :: (LayerBase a) => a -> (Touch -> IO ()) -> IO ()
setOnTouchEnded a f =
  c_setOnTouchEnded a (\i -> c_touchToTouch i >>= f)

setOnTouchCancelled :: (LayerBase a) => a -> (Touch -> IO ()) -> IO ()
setOnTouchCancelled a f =
  c_setOnTouchCancelled a (\i -> c_touchToTouch i >>= f)

setOnTouchesBegan :: (LayerBase a) => a -> ([Touch] -> IO Bool) -> IO ()
setOnTouchesBegan a f =
  c_setOnTouchesBegan a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesMoved :: (LayerBase a) => a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesMoved a f =
  c_setOnTouchesMoved a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesEnded :: (LayerBase a) => a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesEnded a f =
  c_setOnTouchesEnded a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesCancelled :: (LayerBase a) => a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesCancelled a f =
  c_setOnTouchesCancelled a (\is -> c_touchesToTouchList is >>= f)

setOnMouseDragged :: (LayerBase a) => a -> ((Double,Double) -> IO ()) -> IO ()
setOnMouseDragged a f = c_setOnMouseDragged a (\p -> pointToTuple p >>= f)

setOnMouseUp :: (LayerBase a) => a -> ((Double,Double) -> IO ()) -> IO ()
setOnMouseUp a f = c_setOnMouseUp a (\p ->pointToTuple p >>= f)
  

foreign import cpattern "%1.onTouchBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" c_setOnTouchBegan :: (LayerBase a) => a -> (Ptr CTouch -> IO Bool) -> IO ()
foreign import cpattern "%1.onTouchMoved = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchMoved :: (LayerBase a) => a -> (Ptr CTouch -> IO ()) -> IO ()
foreign import cpattern "%1.onTouchCancelled = function(t,_) {A(%2,[[1,t],0]);}"          c_setOnTouchCancelled :: (LayerBase a) => a -> (Ptr CTouch -> IO ()) -> IO ()
foreign import cpattern "%1.onTouchEnded = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchEnded :: (LayerBase a) => a -> (Ptr CTouch -> IO ()) -> IO ()

foreign import cpattern "%1.onTouchesBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" c_setOnTouchesBegan :: (LayerBase a) => a -> (Ptr CTouchSet -> IO Bool) -> IO ()
foreign import cpattern "%1.onTouchesMoved = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchesMoved :: (LayerBase a) => a -> (Ptr CTouchSet -> IO ()) -> IO ()
foreign import cpattern "%1.onTouchesCancelled = function(t,_) {A(%2,[[1,t],0]);}"          c_setOnTouchesCancelled :: (LayerBase a) => a -> (Ptr CTouchSet -> IO ()) -> IO ()
foreign import cpattern "%1.onTouchesEnded = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchesEnded :: (LayerBase a) => a -> (Ptr CTouchSet -> IO ()) -> IO ()

foreign import cpattern "%1.onMouseDragged = function (e) {A(%2,[[1,e.getDelta()],0]);}"       c_setOnMouseDragged :: (LayerBase a) => a -> (Point -> IO ()) -> IO ()
foreign import cpattern "%1.onScrollWheel = function (e) {A(%2,[[1,e.getWheelDelta()],0]);}"   setOnScrollWheel :: (LayerBase a) => a -> (Double -> IO ()) -> IO ()

foreign import cpattern "%1.onMouseUp = function (e) {A(%2,[[1,e.getLocation()],0]);}" c_setOnMouseUp :: (LayerBase a) => a -> (Point -> IO ()) -> IO ()

foreign import cpattern "%1.setTouchEnabled(%2)" setTouchEnabled :: (LayerBase a) => a -> Bool -> IO ()

foreign import cpattern "%1.setMouseEnabled(%2)" setMouseEnabled :: (LayerBase a) => a -> Bool -> IO ()
