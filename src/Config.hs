module Config where

import Types
    
loadConfig :: IO Config
loadConfig = return $ Config { isFullscreen = False
                             , width = 600
                             , height = 600
                             }
               
