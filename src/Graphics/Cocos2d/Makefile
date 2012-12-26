all: TouchDelegate.hs TouchDelegate.js

TouchDelegate.hs: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate

TouchDelegate.js: TouchDelegate-ffi.hs
	haste-ffi-parser TouchDelegate-ffi.hs TouchDelegate
