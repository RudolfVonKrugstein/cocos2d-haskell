module Graphics.Cocos2d.Utils where

import Haste
import Haste.Prim
import Data.Functor
import Data.Word

{- this header defines util function and types needed in many other places
 -}

data CSize
type Size = Ptr CSize
data CPoint
type Point = Ptr CPoint
data CPointArray
type PointArray = Ptr CPointArray

-- color types
data Color4b = Color4b Word8 Word8 Word8 Word8
data DeltaColor4b = DeltaColor4b Int Int Int Int

-- general settings
foreign import cpattern "cc.Director.getInstance().setDisplayStats(%1)" setDisplayStats :: Bool -> IO ()
foreign import cpattern "cc.Director.getInstance().setAnimationInterval(%1)" setAnimationInterval :: Double -> IO ()

-- output some logging info
foreign import cpattern "cc.log(%1)" logOut :: String -> IO ()

-- size type
foreign import cpattern "%1.width"  sizeWidth  :: Size -> IO Double
foreign import cpattern "%1.height" sizeHeight :: Size -> IO Double

sizeToTuple :: Size -> IO (Double,Double)
sizeToTuple s = do
  w <- sizeWidth s
  h <- sizeHeight s
  return (w,h)

foreign import cpattern "cc.Director.getInstance().getWinSize()" jsGetWinSize :: IO Size

getWinSize :: IO (Double,Double)
getWinSize = jsGetWinSize >>= sizeToTuple

-- point type
foreign import cpattern "%1.x"        pointX      :: Point -> IO Double
foreign import cpattern "%1.y"        pointY      :: Point -> IO Double
foreign import cpattern "cc.p(%1,%2)" createPoint :: Double -> Double -> IO Point
tupleToPoint :: (Double,Double) -> IO Point
tupleToPoint t = createPoint (fst t) (snd t)
pointToTuple :: Point -> IO (Double,Double)
pointToTuple p = do
  x <- pointX p
  y <- pointY p
  return (x,y)

foreign import cpattern "cc.PointZero()" jsPointZero :: IO Point
pointZero :: IO (Double,Double)
pointZero = jsPointZero >>= pointToTuple

-- array of points
foreign import cpattern "new Array"            createPointArray :: IO PointArray
foreign import cpattern "%1.push(cc.p(%2,%3))" pushToPointArray :: PointArray -> Double -> Double -> IO ()
pointArrayFromList :: [(Double,Double)] -> IO PointArray
pointArrayFromList is = do
  pa <- createPointArray
  mapM_ (\(x,y) -> pushToPointArray pa x y) is
  return pa

