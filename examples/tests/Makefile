JSFILES = ../../javascript/cocos2d-haskell.js,../../src/Graphics/Cocos2d.js,../../src/Graphics/Cocos2d/TouchDelegate.js,../../src/Graphics/Cocos2d/Scene.js

Main.js: ../../src/Graphics/Cocos2d.hs ../../src/Graphics/Cocos2d.js ../../src/Graphics/Cocos2d/TouchDelegate.hs ../../src/Graphics/Cocos2d/TouchDelegate.js ../../javascript/cocos2d-haskell.js Main.hs ActionManagerTest.hs ../../src/Graphics/Cocos2d/Scene.hs ../../src/Graphics/Cocos2d/Scene.js
	hastec --with-js=$(JSFILES) Main.hs -i../../src --start=asap -XTypeSynonymInstances -XFlexibleInstances -XRankNTypes --out=tests.js --debug
