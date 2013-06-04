{- Main module. Reexports all modules
 - and defines startup function.
 -}

module Graphics.Cocos2d
  ( -- * Modules
    module Graphics.Cocos2d.Action
  , module Graphics.Cocos2d.Animation
  , module Graphics.Cocos2d.Node
  , module Graphics.Cocos2d.Scene
  , module Graphics.Cocos2d.Layer
  , module Graphics.Cocos2d.Utils
  , module Graphics.Cocos2d.Sprite
  , module Graphics.Cocos2d.Label
  , module Graphics.Cocos2d.Menu
  , App
  , cocos2dApp
  , quit
) where

import Graphics.Cocos2d.Action
import Graphics.Cocos2d.Animation
import Graphics.Cocos2d.Node
import Graphics.Cocos2d.Scene
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Utils
import Graphics.Cocos2d.Sprite
import Graphics.Cocos2d.Label
import Graphics.Cocos2d.Menu
import Foreign.Ptr
import Haste
import Haste.Prim

data CApp
type App = Ptr CApp

-- Start cococs2d app
foreign import cpattern "startCocos2dApp(function (a) {A(%1, [[1,a],0]);})" cocos2dApp :: (App -> IO ()) -> IO ()
foreign import cpattern "history.go(-1)" quit :: IO ()
