module TouchDelegate where

data Touch = Touch {
               touchLocation :: (Double,Double),
               touchPrevLocation :: (Double,Double),
               touchViewLocation :: (Double,Double),
               touchPrevViewLocation :: (Double,Double)
             }

-- Foreign type
data TouchDelegateType
type TouchDelegate = Ptr TouchDelegateType
data ForeignTouchType
type ForeignTouch = Ptr ForeignTouchType

-- convert foreign touch
foreign import jscall "%1.getLocation()"               touchGetLocation :: ForeignTouch -> IO Point
foreign import jscall "%1.getPreviousLocation()"       touchGetPreviousLocation :: ForeignTouch -> IO Point
foreign import jscall "%1.getLocationInView()"         touchGetLocationInView :: ForeignTouch -> IO Point
foreign import jscall "%1.getPreviousLocationInView()" touchGetPreviousLocationInView :: ForeignTouch -> IO Point

foreignTouchToTouch :: ForeignTouch -> IO Touch
foreignTouchToTouch foreign = Touch <$> (pointToTuple <$> touchGetLocation foreign)
                                    <*> (pointToTuple  <$> touchGetPreviousLocation foreign)
                                    <*> (pointToTuple <$> touchGetLocationInView foreign)
                                    <*> (pointToTuple <$> touchGetPreviousLocationInView foreign)

class TouchDelegateDerived a where
  toTouchDelegate :: a-> TouchDelegate

  setOnTouchBegan :: a -> (Touch -> IO Bool) -> IO ()
  setOnTouchBegan a f =
    touchDelegateSetOnTouchBegan (toTouchDelegate a) (foreignTouchToTouch >>= f)

foreign import jscall "%1.ccTouchBegan = function(t,_) {return A(%2,[[1,t],0])[2][1];}" touchDelegateSetOnTouchBegan TouchDelegate -> (ForeignTouch -> IO Bool) -> IO ()
