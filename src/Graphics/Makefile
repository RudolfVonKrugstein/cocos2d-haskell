DIRS=Cocos2d

all: compile Cocos2d.hs Cocos2d.js

compile:
		for i in $(DIRS); do make -C $$i; done

%.hs: %-ffi.hs
	haste-ffi-parser $< $(basename $@)

%.js: %.ffi.hs
	haste-ffi-parser $< $(basename $@)
