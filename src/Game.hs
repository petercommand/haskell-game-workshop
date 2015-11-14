{-# LANGUAGE RecursiveDo #-}
module Game where
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Data.Map as M
import Input
import Types
import Rendering
import Graphics.Gloss
import Graphics.Gloss.Rendering

import GHC.Float
import FRP.Elerea.Simple
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, join)
import Control.Monad.Fix (fix)
import Data.Monoid
import Debug.Trace
import Data.Time.Clock

playerSpeed :: Double
playerSpeed = 100.0

initObjs :: M.Map ObjectName GameObject
initObjs = M.fromList [ ("player", def { objPos = (0,0), objRot = Degree 0, objRender = Circle 5.0 })
                      , ("boxes", GameObjects [])
                      ]

initStatus :: GameStatus
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
      initTime <- getCurrentTime
      game <- start $ gameUpdate win userInput glossState config initTime
      fix $ \loop -> do
               getUserInput win userInputSink
               join game
               gameAction
               statusAction
               threadDelay 20000
               loop


gameUpdate :: Window -> Signal UserInput -> State -> Config -> UTCTime -> SignalGen (Signal (IO ())) 
gameUpdate win userInput glossState config initTime = do
  time <- effectful getCurrentTime
  lastTime <- delay initTime time
  let deltaTime = diffUTCTime <$> time <*> lastTime
  result <- transfer2 (initObjs, initStatus) ruleUpdate userInput deltaTime
  return $ do
    action <- renderGame win glossState config <$> result
    status <- processStatus <$> result
    return (action >> status)


processStatus :: (M.Map ObjectName GameObject, GameStatus) -> IO ()
processStatus (_, status) = case status of
                              GameExit -> exitSuccess
                              _ -> return ()
  


ruleUpdate :: UserInput -> NominalDiffTime -> (M.Map ObjectName GameObject, GameStatus) -> (M.Map ObjectName GameObject, GameStatus)
ruleUpdate userInput dt init = foldr (\f acc -> f acc userInput dt) init rules

rules :: [Rule]
rules = map (\(ToRuleFunc f) -> generateRule f) [ ToRuleFunc $ ObjectRule "player" playerMoveRule
                                                , ToRuleFunc $ CollisionRule collisionRule
                                                , ToRuleFunc $ StatusRule exitRule
                                                ]




playerMoveRule :: ObjectRuleFunc
playerMoveRule (obj@(GameObject {}), status) userInput dt =
    let
        t = realToFrac dt
        newObj = case directions userInput of
                   Directions True _ _ _ -> moveObj obj (0, playerSpeed * t)
                   Directions _ True _ _ -> moveObj obj (0, -playerSpeed * t)
                   Directions _ _ True _ -> moveObj obj (-playerSpeed * t, 0)
                   Directions _ _ _ True -> moveObj obj (playerSpeed * t, 0)
                   Directions False False False False -> obj
    in (newObj, status)
playerMoveRule x _ _ = x

exitRule :: StatusRuleFunc
exitRule status userInput =
    let
        keyMap = pressedKeys userInput
        ctrlC = case ctrlPressed keyMap of
                  True -> case keyPressed keyMap Key'C of
                            True -> True
                            False -> False
                  False -> False
    in
      case ctrlC of
        True -> GameExit
        False -> status
        
                                          
moveObj :: GameObject -> (Double, Double) -> GameObject
moveObj obj@(GameObject {}) (dX, dY) = let (x, y) = objPos obj
                                       in obj { objPos = (x + dX, y + dY) }
moveObj (GameObjects x) delta = GameObjects (map (\x -> moveObj x delta) x)
