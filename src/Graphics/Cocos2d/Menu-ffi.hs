{-# LANGUAGE GADTs#-}

module Graphics.Cocos2d.Menu (
  Menu,
  MenuItem,
  MenuItemBase (..),
  MenuItemSprite,
  MenuItemSpriteBase (..),
  MenuItemLabel,
  createMenu,
  createMenuWithItems,
  addMenuItem,
  createMenuItemImage,
  createMenuItemLabel
) where

import Haste
import Haste.Prim
import Graphics.Cocos2d.Node
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Label

data CMenu
type Menu = Ptr CMenu

instance LayerBase Menu where
  toLayer = menuToLayer
foreign import ccall "returnSame" menuToLayer :: Menu -> Layer
instance NodeBase Menu where
  toNode = menuToNode
foreign import ccall "returnSame" menuToNode :: Menu -> Node

data CMenuItem
type MenuItem = Ptr CMenuItem

instance NodeBase MenuItem where
  toNode = menuItemToNode
foreign import ccall "returnSame" menuItemToNode :: MenuItem -> Node

class MenuItemBase a where
  toMenuItem :: a -> MenuItem

instance MenuItemBase MenuItem where
  toMenuItem = id

data CMenuItemSprite
type MenuItemSprite = Ptr CMenuItemSprite
instance MenuItemBase MenuItemSprite where
  toMenuItem = menuItemSpriteToMenuItem
foreign import ccall "returnSame" menuItemSpriteToMenuItem :: MenuItemSprite -> MenuItem
instance NodeBase MenuItemSprite where
  toNode = menuItemToNode . menuItemSpriteToMenuItem

class MenuItemSpriteBase a where
  toMenuItemSprite :: a -> MenuItemSprite
instance MenuItemSpriteBase MenuItemSprite where
  toMenuItemSprite = id

data CMenuItemLabel
type MenuItemLabel = Ptr CMenuItemLabel
instance MenuItemBase MenuItemLabel where
  toMenuItem = menuItemLabelToMenuItem
foreign import ccall "returnSame" menuItemLabelToMenuItem :: MenuItemLabel -> MenuItem
instance NodeBase MenuItemLabel where
  toNode = menuItemToNode . menuItemLabelToMenuItem

data CMenuItemImage
type MenuItemImage = Ptr CMenuItemImage
instance MenuItemSpriteBase MenuItemImage where
  toMenuItemSprite = menuItemImageToMenuItemSprite
foreign import ccall "returnSame" menuItemImageToMenuItemSprite :: MenuItemImage -> MenuItemSprite
instance MenuItemBase MenuItemImage where
  toMenuItem = toMenuItem . toMenuItemSprite
instance NodeBase MenuItemImage where
  toNode = toNode . menuItemImageToMenuItemSprite

foreign import cpattern "cc.Menu.create()" createMenu :: IO Menu

createMenuWithItems :: [MenuItem] -> IO Menu
createMenuWithItems items = do
  m <- createMenu
  mapM_ (\i -> addMenuItem m i) items
  return m

foreign import cpattern "%1.addChild(%2)" addMenuItem :: (MenuItemBase a) => Menu -> a -> IO ()

foreign import cpattern "cc.MenuItemImage.create(%1,%2,function() {A(%3,[0]);},0)" createMenuItemImage :: String -> String -> IO () -> IO MenuItemImage

foreign import cpattern "cc.MenuItemLabel.create(%1,function() {A(%2,[0]);},0)" createMenuItemLabel :: LabelTTF -> IO () -> IO MenuItemLabel

