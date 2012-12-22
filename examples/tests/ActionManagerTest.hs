module ActionManagerTest where

data ActionManagerTest = AMT { prev :: IO Scene,
                               this :: IO Scene,
                               next :: IO Scene,
                               title :: String}
        
