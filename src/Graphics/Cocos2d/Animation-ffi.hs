module Graphics.Cocos2d.Animation where

import Haste
import Haste.Prim

data Animation = ImageFileAnimation [String] Double Bool -- Animation with list of image files, and "restore original frame"
               | CacheAnimation String                   -- Animation from animation cache
               | Loop Int Animation                      -- Loop an animation


data JSAnimationType
type JSAnimation = Ptr JSAnimationType

toJSAnimation :: Animation -> IO JSAnimation
toJSAnimation (ImageFileAnimation names dpu rof) = do
      a <- createJSAnimation
      mapM_ (addAnimationSpriteFrameWithFile a) names
      animationSetDelayPerUnit a dpu
      animationSetRestoreOriginalFrame a rof
      return a
toJSAnimation (CacheAnimation name) = jSAnimationGetFromCache name
toJSAnimation (Loop n a) = do
  a1 <- toJSAnimation a
  animationSetLoops a1 n
  return a1
    
    

foreign import cpattern "cc.Animation.create()"          createJSAnimation :: IO JSAnimation
foreign import cpattern "%1.addSpriteFrameWithFile(%2)"  addAnimationSpriteFrameWithFile :: JSAnimation -> String -> IO ()
foreign import cpattern "%1.setDelayPerUnit(%2)"         animationSetDelayPerUnit :: JSAnimation -> Double -> IO ()
foreign import cpattern "%1.setLoops(%2)"                animationSetLoops :: JSAnimation -> Int -> IO ()
foreign import cpattern "%1.setRestoreOriginalFrame(%2)" animationSetRestoreOriginalFrame :: JSAnimation -> Bool -> IO ()
foreign import cpattern "cc.AnimationCache.getInstance().addAnimations(%1)" loadAnimationsIntoCache :: String -> IO ()
foreign import cpattern "cc.AnimationCache.getInstance().getAnimation(%1)" jSAnimationGetFromCache :: String -> IO JSAnimation

