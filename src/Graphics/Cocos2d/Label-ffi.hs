module Graphics.Cocos2d.Label (
  LabelTTF,
  createLabelTTF
) where

import Haste
import Haste.Prim

import Graphics.Cocos2d.Sprite

data CLabelTTF a
type LabelTTF a = Sprite (CLabelTTF a)

foreign import jscall "cc.LabelTTF.create(%1,%2,%3)" createLabelTTF :: String -> String -> Int -> IO (LabelTTF ())
