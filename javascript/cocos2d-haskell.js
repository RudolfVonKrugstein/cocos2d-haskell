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
        cc.AppController.shareAppController().didFinishLaunchingWithOptions();
	},
	applicationDidFinishLaunching:function () {
      // initialize director
      var director = cc.Director.getInstance();

      var screenSize = cc.EGLView.getInstance().getFrameSize();
      var resourceSize = cc.size(800, 450);
      var designSize = cc.size(800, 450);

      var searchPaths = [];
      var resDirOrders = [];

      searchPaths.push("res");
      cc.FileUtils.getInstance().setSearchPaths(searchPaths);

      cc.FileUtils.getInstance().setSearchResolutionsOrder(resDirOrders);

      director.setContentScaleFactor(resourceSize.width / designSize.width);

      cc.EGLView.getInstance().setDesignResolutionSize(designSize.width, designSize.height, cc.RESOLUTION_POLICY.SHOW_ALL);

      // turn on display FPS
      director.setDisplayStats(this.config['showFPS']);

      // set FPS. the default value is 1.0/60 if you don't call this
      director.setAnimationInterval(1.0 / this.config['frameRate']);

      cc.LoaderScene.preload(g_ressources, function() {
        this.startFun(this);
      }, this);

      return true;
	}
		
});

var myApp = 0;
function startCocos2dApp(start) {
	myApp = new cocos2dApp(start)
};

// Function for casting to different types
function returnSame(i,_) {
	return i;
}

//Scene for haskell with onEnter function
var HaskellScene = cc.Scene.extend({
  ctor:function (f) {
    this._super();
    this.funOnEnter = f;
  },
  onEnter:function() {
    this._super();
    this.funOnEnter(this);
  }
});
