module Graphics.Cocos2d.Action where

import Haste
import Haste.Prim

-- Actions
data Action = Sequence [Action]
            | RotateTo Double Double
            | RotateBy Double Double
            | ScaleTo Double (Double,Double)
            | ScaleBy Double (Double,Double)
            | MoveTo Double (Double,Double)
            | MoveBy Double (Double,Double)
            | DelayTime Double
            | FadeIn Double
            | FadeOut Double
            | CallFunc (IO ())
            | TagAction Int Action
            | Reverse Action

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
toJSAction (RotateTo t r)      = rotateToAction t r
toJSAction (RotateBy t r)      = rotateByAction t r
toJSAction (ScaleTo t (x,y))   = scaleToAction t x y
toJSAction (ScaleBy t (x,y))   = scaleByAction t x y
toJSAction (MoveTo t (x,y))    = moveToAction t x y
toJSAction (MoveBy t (x,y))    = moveByAction t x y
toJSAction (DelayTime t)       = delayTimeAction t
toJSAction (FadeIn t)          = fadeInAction t
toJSAction (FadeOut t)         = fadeOutAction t
toJSAction (CallFunc f)        = callFuncAction f
toJSAction (TagAction id a)    = do
  a1 <- toJSAction a
  tagAction a1 id
  return a1
toJSAction (Reverse a) = do
  a1 <- toJSAction a
  reverseAction a1

foreign import jscall "cc.Sequence.create(%1,%2)"        sequenceAction :: JSAction -> JSAction -> IO JSAction
foreign import jscall "cc.RotateTo.create(%1,%2)"        rotateToAction :: Double -> Double -> IO JSAction
foreign import jscall "cc.RotateBy.create(%1,%2)"        rotateByAction :: Double -> Double -> IO JSAction
foreign import jscall "cc.ScaleTo.create(%1,%2,%3)"      scaleToAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.ScaleBy.create(%1,%2,%3)"      scaleByAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.MoveTo.create(%1,cc.p(%2,%3))" moveToAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.MoveBy.create(%1,cc.p(%2,%3))" moveByAction :: Double -> Double -> Double -> IO JSAction
foreign import jscall "cc.DelayTime.create(%1)"          delayTimeAction :: Double -> IO JSAction
foreign import jscall "cc.FadeIn.create(%1)"             fadeInAction :: Double -> IO JSAction
foreign import jscall "cc.FadeOut.create(%1)"            fadeOutAction :: Double -> IO JSAction
foreign import jscall "cc.CallFunc.create(function (a) {A(%1, [[1,a],0]);})" callFuncAction :: IO () -> IO JSAction
foreign import jscall "%1.setTag(%2)"                    tagAction :: JSAction -> Int -> IO ()
foreign import jscall "%1.reverse()"                     reverseAction :: JSAction -> IO JSAction

