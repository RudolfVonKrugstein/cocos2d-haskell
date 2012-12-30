module Graphics.Cocos2d.Animation where

import Haste
import Haste.Prim

data Animation = ImageFileAnimation [String] Double Int Bool -- Animation with list of image files, number of loops and "restore original frame"
               | CacheAnimation String                       -- Animation from animation cache


data JSAnimationType
type JSAnimation = Ptr JSAnimationType

toJSAnimation :: Animation -> IO JSAnimation
toJSAnimation (ImageFileAnimation names dpu loop rof) = do
      a <- createJSAnimation
      mapM_ (addAnimationSpriteFrameWithFile a) names
      animationSetDelayPerUnit a dpu
      animationSetLoops a loop
      animationSetRestoreOriginalFrame a rof
      return a
toJSAnimation (CacheAnimation name) = jSAnimationGetFromCache name
    
    

foreign import jscall "cc.Animation.create()"          createJSAnimation :: IO JSAnimation
foreign import jscall "%1.addSpriteFrameWithFile(%2)"  addAnimationSpriteFrameWithFile :: JSAnimation -> String -> IO ()
foreign import jscall "%1.setDelayPerUnit(%2)"         animationSetDelayPerUnit :: JSAnimation -> Double -> IO ()
foreign import jscall "%1.setLoops(%2)"                animationSetLoops :: JSAnimation -> Int -> IO ()
foreign import jscall "%1.setRestoreOriginalFrame(%2)" animationSetRestoreOriginalFrame :: JSAnimation -> Bool -> IO ()
foreign import jscall "cc.AnimationCache.getInstance().addAnimations(%1)" loadAnimationsIntoCache :: String -> IO ()
foreign import jscall "cc.AnimationCache.getInstance().getAnimation(%1)" jSAnimationGetFromCache :: String -> IO JSAnimation

