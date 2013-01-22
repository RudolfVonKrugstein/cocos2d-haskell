all: Utils.hs Utils.js Node.hs Node.js TouchDelegate.hs TouchDelegate.js Scene.hs Scene.js Action.hs Action.js Animation.hs Animation.js

%.hs: %-ffi.hs
	haste-ffi-parser $< $(basename $@)

%.js: %.ffi.hs
	haste-ffi-parser $< $(basename $@)
