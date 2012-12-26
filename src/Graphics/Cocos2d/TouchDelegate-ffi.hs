module Graphics.Cocos2d.TouchDelegate where

import Haste
import Haste.Prim
import Graphics.Cocos2d
import Data.Functor
import Control.Applicative
import Data.VectorSpace

data Touch = Touch {
               touchLocation :: (Double,Double),
               touchPrevLocation :: (Double,Double)
             }
touchDelta :: Touch -> (Double,Double)
touchDelta (Touch l pl) = l ^-^ pl

-- Foreign type
data TouchDelegateType
type TouchDelegate = Ptr TouchDelegateType
data ForeignTouchType
type ForeignTouch = Ptr ForeignTouchType
data ForeignTouchSetType
type ForeignTouchSet = Ptr ForeignTouchSetType

-- convert foreign touch
foreign import jscall "%1.getLocation()"               touchGetLocation :: ForeignTouch -> IO Point
foreign import jscall "%1.getPreviousLocation()"       touchGetPreviousLocation :: ForeignTouch -> IO Point

foreignTouchToTouch :: ForeignTouch -> IO Touch
foreignTouchToTouch ft = Touch <$> (pointToTuple <$> touchGetLocation ft)
                                    <*> (pointToTuple  <$> touchGetPreviousLocation ft)

foreignTouchesToTouchList :: ForeignTouchSet -> IO [Touch]
foreignTouchesToTouchList set = do
  num <- touchSetGetNumTouches set
  mapM (\i -> touchSetGetNthTouch set i >>= foreignTouchToTouch) [0..(num-1)]

foreign import jscall "%1.length" touchSetGetNumTouches :: ForeignTouchSet -> IO Int
foreign import jscall "%1[%2]" touchSetGetNthTouch :: ForeignTouchSet -> Int -> IO ForeignTouch

class TouchDelegateDerived a where
  toTouchDelegate :: a -> TouchDelegate

  setOnTouchBegan :: a -> (Touch -> IO Bool) -> IO ()
  setOnTouchBegan a f =
    touchDelegateSetOnTouchBegan (toTouchDelegate a) (\i -> foreignTouchToTouch i >>= f)

  setOnTouchMoved :: a -> (Touch -> IO ()) -> IO ()
  setOnTouchMoved a f =
    touchDelegateSetOnTouchMoved (toTouchDelegate a) (\i -> foreignTouchToTouch i >>= f)

  setOnTouchEnded :: a -> (Touch -> IO ()) -> IO ()
  setOnTouchEnded a f =
    touchDelegateSetOnTouchEnded (toTouchDelegate a) (\i -> foreignTouchToTouch i >>= f)

  setOnTouchCancelled :: a -> (Touch -> IO ()) -> IO ()
  setOnTouchCancelled a f =
    touchDelegateSetOnTouchCancelled (toTouchDelegate a) (\i -> foreignTouchToTouch i >>= f)

  setOnTouchesBegan :: a -> ([Touch] -> IO Bool) -> IO ()
  setOnTouchesBegan a f =
    touchDelegateSetOnTouchesBegan (toTouchDelegate a) (\is -> foreignTouchesToTouchList is >>= f)

  setOnTouchesMoved :: a -> ([Touch] -> IO ()) -> IO ()
  setOnTouchesMoved a f =
    touchDelegateSetOnTouchesMoved (toTouchDelegate a) (\is -> foreignTouchesToTouchList is >>= f)

  setOnTouchesEnded :: a -> ([Touch] -> IO ()) -> IO ()
  setOnTouchesEnded a f =
    touchDelegateSetOnTouchesEnded (toTouchDelegate a) (\is -> foreignTouchesToTouchList is >>= f)

  setOnTouchesCancelled :: a -> ([Touch] -> IO ()) -> IO ()
  setOnTouchesCancelled a f =
    touchDelegateSetOnTouchesCancelled (toTouchDelegate a) (\is -> foreignTouchesToTouchList is >>= f)

  setOnMouseDragged :: a -> ((Double,Double) -> IO ()) -> IO ()
  setOnMouseDragged a f = touchDelegateSetOnMouseDragged (toTouchDelegate a) (\p -> f $ pointToTuple p)
  
  setOnScrollWheel :: a -> (Double -> IO ()) -> IO ()
  setOnScrollWheel a f = touchDelegateSetOnScrollWheel (toTouchDelegate a) f

foreign import jscall "%1.onTouchBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" touchDelegateSetOnTouchBegan :: TouchDelegate -> (ForeignTouch -> IO Bool) -> IO ()
foreign import jscall "%1.onTouchMoved = function(t,_) {A(%2,[[1,t],0]);}"              touchDelegateSetOnTouchMoved :: TouchDelegate -> (ForeignTouch -> IO ()) -> IO ()
foreign import jscall "%1.onTouchCancelled = function(t,_) {A(%2,[[1,t],0]);}"          touchDelegateSetOnTouchCancelled :: TouchDelegate -> (ForeignTouch -> IO ()) -> IO ()
foreign import jscall "%1.onTouchEnded = function(t,_) {A(%2,[[1,t],0]);}"              touchDelegateSetOnTouchEnded :: TouchDelegate -> (ForeignTouch -> IO ()) -> IO ()

foreign import jscall "%1.onTouchesBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" touchDelegateSetOnTouchesBegan :: TouchDelegate -> (ForeignTouchSet -> IO Bool) -> IO ()
foreign import jscall "%1.onTouchesMoved = function(t,_) {A(%2,[[1,t],0]);}"              touchDelegateSetOnTouchesMoved :: TouchDelegate -> (ForeignTouchSet -> IO ()) -> IO ()
foreign import jscall "%1.onTouchesCancelled = function(t,_) {A(%2,[[1,t],0]);}"          touchDelegateSetOnTouchesCancelled :: TouchDelegate -> (ForeignTouchSet -> IO ()) -> IO ()
foreign import jscall "%1.onTouchesEnded = function(t,_) {A(%2,[[1,t],0]);}"              touchDelegateSetOnTouchesEnded :: TouchDelegate -> (ForeignTouchSet -> IO ()) -> IO ()

foreign import jscall "%1.onMouseDragged = function (e) {A(%2,[[1,e.getDelta()],0]);}"   touchDelegateSetOnMouseDragged :: TouchDelegate -> (Point -> IO ()) -> IO ()
foreign import jscall "%1.onScrollWheel = function (e) {A(%2,[[1,e.getWheelDelta()],0]);}"   touchDelegateSetOnScrollWheel :: TouchDelegate -> (Double -> IO ()) -> IO ()

instance TouchDelegateDerived Scene where
  toTouchDelegate = sceneToTouchDelegate
foreign import ccall "returnSame" sceneToTouchDelegate :: Scene -> TouchDelegate
