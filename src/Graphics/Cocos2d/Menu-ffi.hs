{-# LANGUAGE GADTs#-}

module Graphics.Cocos2d.Menu (
  Menu,
  MenuItem,
  MenuItemSprite,
  MenuItemLabel,
  createMenu,
  createMenuWithItems,
  addMenuItem,
  createMenuItemImage,
  createMenuItemLabel,
  MenuItemBox(..)
) where

import Haste
import Haste.Prim
import Graphics.Cocos2d.Node
import Graphics.Cocos2d.Layer
import Graphics.Cocos2d.Label

data CMenu a
type Menu a = Layer (CMenu a)

data CMenuItem a
type MenuItem a = Node (CMenuItem a)

data CMenuItemSprite a
type MenuItemSprite a = MenuItem (CMenuItemSprite a)

data CMenuItemLabel a
type MenuItemLabel a = MenuItem (CMenuItemLabel a)

data CMenuItemImage a
type MenuItemImage a = MenuItemSprite (CMenuItemImage a)

foreign import jscall "cc.Menu.create()" createMenu :: IO (Menu ())

data MenuItemBox = forall a. MenuItemBox (MenuItem a)

createMenuWithItems :: [MenuItemBox] -> IO (Menu ())
createMenuWithItems items = do
  m <- createMenu
  mapM_ (\(MenuItemBox i) -> addMenuItem m i) items
  return m

foreign import jscall "%1.addChild(%2)" addMenuItem :: Menu a -> MenuItem b -> IO ()

foreign import jscall "cc.MenuItemImage.create(%1,%2,%3,0)" createMenuItemImage :: String -> String -> IO () -> IO (MenuItemImage ())

foreign import jscall "cc.MenuItemLabel.create(%1,%2,0)" createMenuItemLabel :: LabelTTF a -> IO () -> IO (MenuItemLabel ())

