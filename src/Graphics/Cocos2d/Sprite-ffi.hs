module Graphics.Cocos2d.Sprite (
  Sprite,
  createSprite 
) where

import Haste
import Haste.Prim
import Graphics.Cocos2d.Node
import Graphics.Cocos2d.Utils

data CSprite a
type Sprite a = Node (CSprite a)

foreign import jscall "cc.Sprite.create(%1)" createSprite :: String -> IO (Sprite ())
