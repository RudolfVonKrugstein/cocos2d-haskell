module Graphics.Cocos2d.Label (
  LabelTTF,
  createLabelTTF
) where

import Haste
import Haste.Prim

import Graphics.Cocos2d.Sprite
import Graphics.Cocos2d.Node

data CLabelTTF
type LabelTTF = Ptr CLabelTTF

instance NodeBase LabelTTF where
  toNode = labelTTFToNode
foreign import ccall "returnSame" labelTTFToNode :: LabelTTF -> Node
instance SpriteBase LabelTTF where
  toSprite = labelTTFToSprite
foreign import ccall "returnSame" labelTTFToSprite :: LabelTTF -> Sprite

foreign import cpattern "cc.LabelTTF.create(%1,%2,%3)" createLabelTTF :: String -> String -> Int -> IO LabelTTF
