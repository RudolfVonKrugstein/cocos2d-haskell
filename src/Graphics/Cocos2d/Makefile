all: TouchDelegate.hs TouchDelegate.js Scene.hs Scene.js

TouchDelegate.hs: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate

TouchDelegate.js: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate

Scene.hs: Scene-ffi.hs
	haste-ffi-parser Scene-ffi.hs Scene

Scene.js: Scene-ffi.hs
	haste-ffi-parser Scene-ffi.hs Scene
