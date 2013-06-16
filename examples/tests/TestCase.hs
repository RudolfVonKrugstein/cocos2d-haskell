module TestCase where

import Haste (alert)
import Graphics.Cocos2d
import Graphics.Cocos2d.Scene
import Resources

-- Base structure to structure test cases
data TestCase = TestCase {
	prev     :: TestCase,
	scene    :: IO Scene,
	next     :: TestCase,
	title    :: String}

-- Create test case from list of scenes
testCaseFromList :: [(IO Scene, String)] -> TestCase
testCaseFromList all@((scene,name):rest) = TestCase prevTestCase scene nextTestCase name
  where
  prevTestCase = testCaseFromList $ (last all):(init all)
  nextTestCase = testCaseFromList $ rest ++ [(scene,name)]

