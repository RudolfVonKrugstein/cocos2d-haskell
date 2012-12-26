DIRS=Cocos2d

all: compile Cocos2d.hs Cocos2d.js

compile:
	for i in $(DIRS); do make -C $$i; done

Cocos2d.hs: Cocos2d-ffi.hs
	haste-ffi-parser Cocos2d-ffi.hs Cocos2d

Cocos2d.js: Cocos2d-ffi.hs
	haste-ffi-parser Cocos2d-ffi.hs Cocos2d
