cocos2d-haskell
===============

Bindings to cocox2d-html5 for use with the haste haskell to javascript compiler.

Recently, [cocos2d-x][cocos2dx] has added javascript bindings allowing cocos2d-html5 prorams to run anywhere, where cocos2d-x runs.

So haskell programs written with these bindings should run on all these platforms (has not been tested yet).

To compile cocos2d-haskell:

* Installe [haste][haste] and [haste-ffi-parser][haste-ffi-parser].

* checkout [cocos2d-html5][cocos2d-html5] in the same directory as cocos2d-haskell (so the directory structure should be:

```
parentDir
|-> cocos2d-haskell
|-> cocos2d-html5
```

* go into cocos2-haskell directory and run 'make'

* go up one directory (cd ..) and start a http server (i.E. python -m SimpleHTTPServer).

* place your browser at [127.0.0.1:8000/cocos2d-haskell/examples/tests](127.0.0.1:8000/cocos2d-haskell/examples/tests).

[haste]: https://github.com/valderman/haste-compiler
[cocos2dx]: http://www.cocos2d-x.org/
[haste-ffi-parser]: https://github.com/RudolfVonKrugstein/haste-ffi-parser
[cocos2d-html5]: https://github.com/cocos2d/cocos2d-html5
