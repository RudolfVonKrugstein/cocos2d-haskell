all: TouchDelegate.hs TouchDelegate.js Scene.hs Scene.js Action.hs Action.js

TouchDelegate.hs: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate

TouchDelegate.js: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate

Scene.hs: Scene-ffi.hs
	haste-ffi-parser Scene-ffi.hs Scene

Scene.js: Scene-ffi.hs
	haste-ffi-parser Scene-ffi.hs Scene

Action.hs: Action-ffi.hs
	haste-ffi-parser Action-ffi.hs Action

Action.js: Action-ffi.hs
	haste-ffi-parser Action-ffi.hs Action
