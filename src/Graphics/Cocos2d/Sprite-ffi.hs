module Graphics.Cocos2d.Sprite (
  Sprite,
  SpriteBase (..),
  createSprite 
) where

import Haste
import Haste.Prim
import Graphics.Cocos2d.Node
import Graphics.Cocos2d.Utils

data CSprite
type Sprite = Ptr CSprite

class SpriteBase a where
  toSprite :: a -> Sprite

instance SpriteBase Sprite where
  toSprite = id

instance NodeBase Sprite where
  toNode = spriteToNode

foreign import ccall "returnSame" spriteToNode :: Sprite -> Node

foreign import cpattern "cc.Sprite.create(%1)" createSprite :: String -> IO Sprite
