{-# LANGUAGE RecursiveDo #-}
module Game where
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Data.Map as M
import Input
import Graphics.Gloss
import Graphics.Gloss.Rendering

import FRP.Elerea.Simple
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, join)
import Control.Monad.Fix (fix)
import Data.Monoid
import Debug.Trace
import Types

playerSpeed = 10


initObjs = M.fromList [("player", GameObject { objPos = (0,0), objRender = Circle 5.0 })]

initStatus = GameNotStarted
    
startGame :: Config -> IO ()
startGame config = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow (width config) (height config) "Gizzenburg Game" Nothing Nothing
    case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              go win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
    go :: Window -> IO ()
    go win = do
      (userInput, userInputSink) <- external def
      glossState <- initState
      game <- start $ gameUpdate win userInput glossState config
--      hotkey <- start $ keyCommand
      fix $ \loop -> do
               getUserInput win userInputSink
               join game
--               join hotkey
               threadDelay 20000
               loop
gameUpdate :: Window -> Signal UserInput -> State -> Config -> SignalGen (Signal (IO ())) 
gameUpdate win userInput glossState config = do
  result <- transfer (initObjs, initStatus) ruleUpdate userInput
  return $ renderGame win glossState <$> result


renderGame :: Window -> State -> (M.Map ObjectName GameObject, GameStatus) -> IO ()
renderGame win glossState (objs, status) = do
  displayPicture (500, 500) white glossState 1.0 $
                 Pictures $ map (objRender . snd) $ M.toList objs
  swapBuffers win
  return ()

ruleUpdate :: UserInput -> (M.Map ObjectName GameObject, GameStatus) -> (M.Map ObjectName GameObject, GameStatus)
ruleUpdate userInput init = foldr (\f acc -> f acc userInput) init rules

rules :: [Rule]
rules = map generateRule [("player", playerMove)]

generateRule :: (ObjectName, RuleFunc) -> Rule
generateRule (name, f) = \(map, status) userInput -> case M.lookup name map of
                                                       Just ori -> let (newObj, newStatus) = f (ori, status) userInput
                                                                   in (M.adjust (const newObj) name map, newStatus)
                                                       Nothing -> ("object \"" <> name <> "\" not found in object map") `trace` (map, status)


playerMove :: RuleFunc
playerMove (obj, status) userInput = let newObj = case directions userInput of
                                                    Directions True _ _ _ -> moveObj obj (0, playerSpeed)
                                                    Directions _ True _ _ -> moveObj obj (0, -playerSpeed)
                                                    Directions _ _ True _ -> moveObj obj (-playerSpeed, 0)
                                                    Directions _ _ _ True -> moveObj obj (playerSpeed, 0)
                                                    Directions False False False False -> obj
                                     in (newObj, status)

moveObj :: GameObject -> (Double, Double) -> GameObject
moveObj obj (dX, dY) = let (x, y) = objPos obj
                       in obj { objPos = (x + dX, y + dY) }
