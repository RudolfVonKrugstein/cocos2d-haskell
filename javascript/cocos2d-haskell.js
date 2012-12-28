/* Class for starting cocos2d app
 */

var cocos2dApp = cc.Application.extend({
	config:document['ccConfig'],
	ctor:function(start) {
		this._super();
		this.startFun = start
		cc.COCOS2D_DEBUG  = this.config['COCOS2D_DEBUG'];
		cc.initDebugSetting();
		cc.setup(this.config['tag']);
		cc.Loader.getInstance().onloading = function () {
			cc.LoaderScene.getInstance().draw();
		};
		cc.Loader.getInstance().onload = function () {
			cc.AppController.shareAppController().didFinishLaunchingWithOptions();
		};
		cc.Loader.getInstance().preload(g_ressources);
	},
	applicationDidFinishLaunching:function () {
		this.startFun(this);
		return true;
	}
		
});

var myApp = 0;
function startCocos2dApp(start) {
	myApp = new cocos2dApp(start)
};

// Function for casting to different types
function returnSame(i,_) {
	return [1,0,i];
}

// Function to create scene and add onEnter function
function createHSScene(f) {
  var s = cc.Scene.create();
  s.onEnter = funtion() {A(f,[[s,1],0]);};
  return s;
}

