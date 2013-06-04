all: Utils.hs Utils.js Node.hs Node.js Layer.hs Layer.js Scene.hs Scene.js Action.hs Action.js Animation.hs Animation.js Sprite.hs Sprite.js Label.hs Label.js Menu.hs Menu.js

%.hs: %-ffi.hs
	haste-ffi-parser -c ../ffiConverts -i $< -o $@ -j $(basename $@).js

%.js: %.ffi.hs
	haste-ffi-parser -c ../ffiConverts -i $< -j $@ -o $(basename $@).hs
