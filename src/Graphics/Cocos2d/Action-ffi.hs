module Graphics.Cocos2d.Action where

import Haste
import Haste.Prim
import Graphics.Cocos2d
import Graphics.Cocos2d.Animation
import Data.Word

-- Actions
data Action = Sequence [Action]
            | Spawn [Action]
            | RotateTo Double Double
            | RotateBy Double Double
            | ScaleTo Double (Double,Double)
            | ScaleBy Double (Double,Double)
            | MoveTo Double (Double,Double)
            | MoveBy Double (Double,Double)
            | SkewTo Double (Double,Double)
            | SkewBy Double (Double,Double)
            | JumpTo Double (Double,Double) Double Int
            | JumpBy Double (Double,Double) Double Int
            | BezierTo Double ((Double,Double), (Double,Double), (Double,Double))
            | BezierBy Double ((Double,Double), (Double,Double), (Double,Double))
            | TintTo Double Color4b
            | TintBy Double DeltaColor4b
            | Blink Double Int
            | DelayTime Double
            | FadeIn Double
            | FadeOut Double
            | AnimationAction Animation
            | Place (Double,Double)
            | Show
            | Hide
            | ToggleVisibility
            | OrbitCamera Double Double Double Double Double Double Double
            | Follow Node Double Double Double Double
            | CardinalSplineTo Double [(Double,Double)] Double
            | CardinalSplineBy Double [(Double,Double)] Double
            | CatmullRomTo Double [(Double,Double)]
            | CatmullRomBy Double [(Double,Double)]
            | TargetedAction Node Action
            | CallFunc (IO ())
            | TagAction Int Action
            | Reverse Action
            | Repeat Action Int
            | RepeatForever Action


data JSActionType
type JSAction = Ptr JSActionType

toJSAction :: Action -> IO JSAction
{-toJSAction a = unsafePerformIO $ do
  alert (show a)
  return -}
toJSAction (Sequence (a:b:as)) = do
  a1 <- toJSAction a
  a2 <- toJSAction (Sequence (b:as))
  sequenceAction a1 a2
toJSAction (Sequence [a])      = toJSAction a
toJSAction (Spawn (a:b:as)) = do
  a1 <- toJSAction a
  a2 <- toJSAction (Spawn (b:as))
  spawnAction a1 a2
toJSAction (Spawn [a]) = toJSAction a
toJSAction (RotateTo t r)      = rotateToAction t r
toJSAction (RotateBy t r)      = rotateByAction t r
toJSAction (ScaleTo t (x,y))   = scaleToAction t x y
toJSAction (ScaleBy t (x,y))   = scaleByAction t x y
toJSAction (MoveTo t (x,y))    = moveToAction t x y
toJSAction (MoveBy t (x,y))    = moveByAction t x y
toJSAction (SkewTo t (x,y))    = skewToAction t x y
toJSAction (SkewBy t (x,y))    = skewByAction t x y
toJSAction (JumpTo t (x,y) h n)= jumpToAction t x y h n
toJSAction (JumpBy t (x,y) h n)= jumpByAction t x y h n
toJSAction (BezierTo t ((x1,y1),(x2,y2),(x3,y3))) = bezierToAction t x1 y1 x2 y2 x3 y3
toJSAction (BezierBy t ((x1,y1),(x2,y2),(x3,y3))) = bezierByAction t x1 y1 x2 y2 x3 y3
toJSAction (TintTo t (Color4b r g b a)) = tintToAction t r g b a
toJSAction (TintBy t (DeltaColor4b r g b a)) = tintByAction t r g b a
toJSAction (DelayTime t)       = delayTimeAction t
toJSAction (FadeIn t)          = fadeInAction t
toJSAction (FadeOut t)         = fadeOutAction t
toJSAction (AnimationAction a) = do
  anim <- toJSAnimation a
  animationAction anim
toJSAction (Place (x,y))      = placeAction x y
toJSAction Show                = showAction
toJSAction Hide                = hideAction
toJSAction ToggleVisibility    = toggleVisibilityAction
toJSAction (Blink t n)         = blinkAction t n
toJSAction (CallFunc f)        = callFuncAction f
toJSAction (TagAction id a)    = do
  a1 <- toJSAction a
  tagAction a1 id
  return a1
toJSAction (Reverse a) = do
  a1 <- toJSAction a
  reverseAction a1
toJSAction (Repeat a n) = do
  a1 <- toJSAction a
  repeatAction a1 n
toJSAction (RepeatForever a) = toJSAction a >>= repeatForeverAction
toJSAction (OrbitCamera t radius deltaRadius angleZ deltaAngleZ angleX deltaAngleX)
  = orbitCameraAction t radius deltaRadius angleZ deltaAngleZ angleX deltaAngleX
toJSAction (Follow n x y w h) = followAction n x y w h
toJSAction (CardinalSplineTo t ps ten) = do
  arr <- pointArrayFromList ps
  cardinalSplineToAction t arr ten
toJSAction (CardinalSplineBy t ps ten) = do
  arr <- pointArrayFromList ps
  cardinalSplineByAction t arr ten
toJSAction (CatmullRomTo t ps) = do
  arr <- pointArrayFromList ps
  catmullRomToAction t arr
toJSAction (CatmullRomBy t ps) = do
  arr <- pointArrayFromList ps
  catmullRomByAction t arr
toJSAction (TargetedAction n a) = do
  a1 <- toJSAction a
  targetedAction n a1

foreign import jscall "cc.Sequence.create(%1,%2)"        sequenceAction :: JSAction -> JSAction -> IO JSAction
foreign import jscall "cc.Spawn.create(%1,%2)"           spawnAction :: JSAction -> JSAction -> IO JSAction
foreign import jscall "cc.RotateTo.create(%1,%2)"        rotateToAction :: Double -> Double -> IO JSAction
foreign import jscall "cc.RotateBy.create(%1,%2)"        rotateByAction :: Double -> Double -> IO JSAction
foreign import jscall "cc.ScaleTo.create(%1,%2,%3)"      scaleToAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.ScaleBy.create(%1,%2,%3)"      scaleByAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.MoveTo.create(%1,cc.p(%2,%3))" moveToAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.MoveBy.create(%1,cc.p(%2,%3))" moveByAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.SkewTo.create(%1,%2,%3)" skewToAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.SkewBy.create(%1,%2,%3)" skewByAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.JumpTo.create(%1,cc.p(%2,%3),%4,%5)" jumpToAction :: Double -> Double -> Double -> Double -> Int -> IO JSAction
foreign import jscall "cc.JumpBy.create(%1,cc.p(%2,%3),%4,%5)" jumpByAction :: Double -> Double -> Double -> Double -> Int -> IO JSAction
foreign import jscall "cc.BezierTo.create(%1,[cc.p(%2,%3),cc.p(%4,%5),cc.p(%6,%7)])" bezierToAction :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.BezierBy.create(%1,[cc.p(%2,%3),cc.p(%4,%5),cc.p(%6,%7)])" bezierByAction :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.TintTo.create(%1,%2,%3,%4,%5)" tintToAction :: Double -> Word8 -> Word8 -> Word8 -> Word8 -> IO JSAction
foreign import jscall "cc.TintBy.create(%1,%2,%3,%4,%5)" tintByAction :: Double -> Int -> Int -> Int -> Int -> IO JSAction
foreign import jscall "cc.DelayTime.create(%1)"          delayTimeAction :: Double -> IO JSAction
foreign import jscall "cc.FadeIn.create(%1)"             fadeInAction :: Double -> IO JSAction
foreign import jscall "cc.FadeOut.create(%1)"            fadeOutAction :: Double -> IO JSAction
foreign import jscall "cc.Animate.create(%1)"            animationAction :: JSAnimation -> IO JSAction
foreign import jscall "cc.Place.create(cc.p(%1,%2))"     placeAction :: Double -> Double -> IO JSAction
foreign import jscall "cc.Show.create()"                  showAction :: IO JSAction
foreign import jscall "cc.Hide.create()"                  hideAction :: IO JSAction
foreign import jscall "cc.ToggleVisibility.create()"      toggleVisibilityAction :: IO JSAction
foreign import jscall "cc.Blink.create(%1,%2)" blinkAction :: Double -> Int -> IO JSAction
foreign import jscall "cc.CallFunc.create(function (a) {A(%1, [[1,a],0]);})" callFuncAction :: IO () -> IO JSAction
foreign import jscall "%1.setTag(%2)"                    tagAction :: JSAction -> Int -> IO ()
foreign import jscall "%1.reverse()"                     reverseAction :: JSAction -> IO JSAction
foreign import jscall "cc.Repeat.create(%1,%2)"          repeatAction :: JSAction -> Int -> IO JSAction
foreign import jscall "cc.RepeatForever.create(%1)"      repeatForeverAction :: JSAction -> IO JSAction
foreign import jscall "cc.OrbitCamera.create(%*)"        orbitCameraAction :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.Follow.create(%1,cc.rect(%*))" followAction :: Node -> Double -> Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.CardinalSplineTo.create(%*)" cardinalSplineToAction :: Double -> PointArray -> Double -> IO JSAction
foreign import jscall "cc.CardinalSplineBy.create(%*)" cardinalSplineByAction :: Double -> PointArray -> Double -> IO JSAction
foreign import jscall "cc.CatmullRomTo.create(%*)" catmullRomToAction :: Double -> PointArray -> IO JSAction
foreign import jscall "cc.CatmullRomBy.create(%*)" catmullRomByAction :: Double -> PointArray -> IO JSAction
foreign import jscall "cc.TargetedAction.create(%*)" targetedAction :: Node -> JSAction -> IO JSAction

-- Operations on nodes
addAction :: (NodeDerived a) => Action -> a -> Bool -> IO ()
addAction a n b = do
  a1 <- toJSAction a
  nodeAddAction a1 (toNode n) b

runAction :: (NodeDerived a) => a -> Action -> IO ()
runAction n a = do
  a1 <- toJSAction a
  nodeRunAction (toNode n) $! a1

stopAllActions :: (NodeDerived a) => a -> IO ()
stopAllActions a = nodeStopAllActions (toNode a)

stopActionByTag :: (NodeDerived a) => a -> Int -> IO ()
stopActionByTag a i = nodeStopActionByTag (toNode a) i

foreign import jscall "%1.runAction(%2)" nodeRunAction :: Node -> JSAction -> IO ()
foreign import jscall "cc.Director.getInstance().getActionManager().addAction(%1,%2,%3)" nodeAddAction :: JSAction -> Node -> Bool -> IO ()
foreign import jscall "%1.stopAllActions()" nodeStopAllActions :: Node -> IO ()
foreign import jscall "%1.stopActionByTag(%2)" nodeStopActionByTag :: Node -> Int -> IO ()

foreign import jscall "cc.Director.getInstance().getActionManager().pauseAllRunningActions()" nodePauseAllRunningActions :: IO NodeSet
pauseAllRunningActions :: IO [Node]
pauseAllRunningActions = nodePauseAllRunningActions >>= nodeSetToNodeList
