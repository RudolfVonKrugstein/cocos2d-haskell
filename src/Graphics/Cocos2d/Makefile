all: Utils.hs Utils.js Node.hs Node.js Layer.hs Layer.js Scene.hs Scene.js Action.hs Action.js Animation.hs Animation.js

%.hs: %-ffi.hs
	haste-ffi-parser $< $(basename $@)

%.js: %.ffi.hs
	haste-ffi-parser $< $(basename $@)
