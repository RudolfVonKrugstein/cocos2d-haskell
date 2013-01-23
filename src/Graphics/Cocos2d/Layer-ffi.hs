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

data CLayer a
type Layer a = Node (CLayer a)

foreign import jscall "new cc.Layer()" createLayer :: IO (Layer ())
foreign import jscall "cc.LayerGradient.create(cc.c4b(%1,%2,%3,%4), cc.c4b(%5,%6,%7,%8))" jsCreateLayerGradient :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO (Layer ())
foreign import jscall "cc.LayerColor.create(cc.c4b(%1,%2,%3,%4))" jsCreateLayerColor :: Word8 -> Word8 -> Word8 -> Word8 -> IO (Layer ())
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
foreign import jscall "%1.getLocation()"          c_touchGetLocation         :: Ptr CTouch -> IO Point
foreign import jscall "%1.getPreviousLocation()"  c_touchGetPreviousLocation :: Ptr CTouch -> IO Point

c_touchToTouch :: Ptr CTouch -> IO Touch
c_touchToTouch ft = Touch <$> (c_touchGetLocation ft >>= pointToTuple)
                          <*> (c_touchGetPreviousLocation ft >>= pointToTuple)

c_touchesToTouchList :: Ptr CTouchSet -> IO [Touch]
c_touchesToTouchList set = do
  num <- c_touchSetGetNumTouches set
  mapM (\i -> c_touchSetGetNthTouch set i >>= c_touchToTouch) [0..(num-1)]

foreign import jscall "%1.length" c_touchSetGetNumTouches :: Ptr CTouchSet -> IO Int
foreign import jscall "%1[%2]"    c_touchSetGetNthTouch   :: Ptr CTouchSet -> Int -> IO (Ptr CTouch)

setOnTouchBegan :: Layer a -> (Touch -> IO Bool) -> IO ()
setOnTouchBegan a f =
  c_setOnTouchBegan a (\i -> c_touchToTouch i >>= f)

setOnTouchMoved :: Layer a -> (Touch -> IO ()) -> IO ()
setOnTouchMoved a f =
  c_setOnTouchMoved a (\i -> c_touchToTouch i >>= f)

setOnTouchEnded :: Layer a -> (Touch -> IO ()) -> IO ()
setOnTouchEnded a f =
  c_setOnTouchEnded a (\i -> c_touchToTouch i >>= f)

setOnTouchCancelled :: Layer a -> (Touch -> IO ()) -> IO ()
setOnTouchCancelled a f =
  c_setOnTouchCancelled a (\i -> c_touchToTouch i >>= f)

setOnTouchesBegan :: Layer a -> ([Touch] -> IO Bool) -> IO ()
setOnTouchesBegan a f =
  c_setOnTouchesBegan a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesMoved :: Layer a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesMoved a f =
  c_setOnTouchesMoved a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesEnded :: Layer a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesEnded a f =
  c_setOnTouchesEnded a (\is -> c_touchesToTouchList is >>= f)

setOnTouchesCancelled :: Layer a -> ([Touch] -> IO ()) -> IO ()
setOnTouchesCancelled a f =
  c_setOnTouchesCancelled a (\is -> c_touchesToTouchList is >>= f)

setOnMouseDragged :: Layer a -> ((Double,Double) -> IO ()) -> IO ()
setOnMouseDragged a f = c_setOnMouseDragged a (\p -> pointToTuple p >>= f)
  

foreign import jscall "%1.onTouchBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" c_setOnTouchBegan :: (Layer a) -> (Ptr CTouch -> IO Bool) -> IO ()
foreign import jscall "%1.onTouchMoved = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchMoved :: (Layer a) -> (Ptr CTouch -> IO ()) -> IO ()
foreign import jscall "%1.onTouchCancelled = function(t,_) {A(%2,[[1,t],0]);}"          c_setOnTouchCancelled :: Layer a -> (Ptr CTouch -> IO ()) -> IO ()
foreign import jscall "%1.onTouchEnded = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchEnded :: Layer a -> (Ptr CTouch -> IO ()) -> IO ()

foreign import jscall "%1.onTouchesBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" c_setOnTouchesBegan :: Layer a -> (Ptr CTouchSet -> IO Bool) -> IO ()
foreign import jscall "%1.onTouchesMoved = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchesMoved :: Layer a -> (Ptr CTouchSet -> IO ()) -> IO ()
foreign import jscall "%1.onTouchesCancelled = function(t,_) {A(%2,[[1,t],0]);}"          c_setOnTouchesCancelled :: Layer a -> (Ptr CTouchSet -> IO ()) -> IO ()
foreign import jscall "%1.onTouchesEnded = function(t,_) {A(%2,[[1,t],0]);}"              c_setOnTouchesEnded :: Layer a -> (Ptr CTouchSet -> IO ()) -> IO ()

foreign import jscall "%1.onMouseDragged = function (e) {A(%2,[[1,e.getDelta()],0]);}"       c_setOnMouseDragged :: Layer a -> (Point -> IO ()) -> IO ()
foreign import jscall "%1.onScrollWheel = function (e) {A(%2,[[1,e.getWheelDelta()],0]);}"   setOnScrollWheel :: Layer a -> (Double -> IO ()) -> IO ()

foreign import jscall "%1.setTouchEnabled(%2)" setTouchEnabled :: Layer a -> Bool -> IO ()
