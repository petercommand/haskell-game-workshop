{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
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
import System.Exit ( exitSuccess, exitFailure )
import Control.Concurrent (threadDelay)
import Control.Monad (when, join)
import Control.Monad.Fix (fix)
import Data.Monoid
import Debug.Trace
import Data.Time.Clock
import Data.Function (on)
playerSpeed :: Double
playerSpeed = 100.0

initObjs :: M.Map ObjectName GameObject
initObjs = M.fromList [ ("player", def { objType = Player, objPos = (0,0), objRot = Degree 0, objRender = Circle 5.0 })
                      , ("boxes", def { objPos = (10, 10), objRender = Circle 10.0})
                      ]
initGameState = GameState { objs = initObjs
                          , collisionObjs = M.empty
                          }


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
               threadDelay 20000
               loop


gameUpdate :: Window -> Signal UserInput -> State -> Config -> UTCTime -> SignalGen (Signal (IO ())) 
gameUpdate win userInput glossState config initTime = do
  time <- effectful getCurrentTime
  lastTime <- delay initTime time
  let deltaTime = diffUTCTime <$> time <*> lastTime
  result <- transfer2 (initGameState, initStatus) ruleUpdate userInput deltaTime
  return $ do
    action <- renderGame win glossState config <$> result
    status <- processStatus <$> result
    return (action >> status)


processStatus :: (GameState, GameStatus) -> IO ()
processStatus (_, status) = case status of
                              GameExit GameExitSuccess -> exitSuccess
                              GameExit (GameExitFailure reason) -> do
                                          putStrLn reason
                                          exitFailure
                              _ -> return ()
  


ruleUpdate :: UserInput -> NominalDiffTime -> (GameState, GameStatus) -> (GameState, GameStatus)
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

-- at least O(N^2) implementation
collisionRule :: CollisionRuleFunc
collisionRule r@(gameState, _) userInput dt =
    let
        t = realToFrac dt
        collidable = collisionObjs gameState
        gameObjs = objs gameState
    in M.foldlWithKey' (\state (obj :: String) collisionInfo1 ->
                        let collidedObj = M.foldlWithKey'
                                          (\acc obj2 collisionInfo2 ->
                                               let
                                                   collideWith :: CollisionType -> CollisionType -> Bool
                                                   collideWith (CircleCollision posA radiusA) (CircleCollision posB radiusB) =
                                                       let Position aX aY = posA
                                                           Position bX bY = posB
                                                           distanceSquared =  ((aX - bX) ** 2) + ((aY - bY) ** 2)
                                                       in distanceSquared < (radiusA ** 2) ||
                                                          distanceSquared < (radiusB ** 2)
                                                   collideWith (BoxCollision posA widthA heightA) (BoxCollision posB widthB heightB) =
                                                       let Position aX aY = posA
                                                           Position bX bY = posB
                                                       in ((abs (aX - bX)) * 2 < widthA + widthB) &&
                                                              ((abs (aY - bY)) * 2 < heightA + widthB)
                                                   collideWith (BoxCollision posA widthA heightA) (CircleCollision posB radiusB) =
                                                       let Position aX aY = posA
                                                           Position bX bY = posB
                                                           circleCenterInBox = bX >= aX && bY >= aY && bX <= aX + widthA && bY <= aY + heightA
                                                           distanceSquared (Position x1 y1) (Position x2 y2) =  ((x1 - x2) ** 2) + ((y1 - y2) ** 2)
                                                           intersectCircle pos1 pos2 radius = distanceSquared pos1 pos2 < (radius ** 2)
                                                       in circleCenterInBox ||
                                                          intersectCircle posA posB radiusB ||
                                                          intersectCircle (Position (aX + heightA) aY) posB radiusB ||
                                                          intersectCircle (Position aX (aY + widthA)) posB radiusB ||
                                                          intersectCircle (Position (aX + heightA) (aY + widthA)) posB radiusB
                                                   collideWith a@(CircleCollision _ _) b@(BoxCollision _ _ _) = collideWith b a
                                                   collided :: Bool
                                                   collided = collideWith (collisionInfo collisionInfo1) (collisionInfo collisionInfo2)
                                               in if collided && (obj2 /= obj)
                                                  then obj2:acc
                                                  else acc
                                          ) [] collidable
                        in foldl (\acc obj2 -> case (M.lookup obj2 collidable) >>= collisionCallback of
                                                 Just (CollisionCallback callback) -> callback obj2 obj acc
                                                 Nothing -> acc
                                 ) state collidedObj
                     ) r collidable
                       
exitRule :: StatusRuleFunc
exitRule status userInput =
    let
        keyMap = pressedKeys userInput
        ctrlC = ctrlPressed keyMap && keyPressed keyMap Key'C 
    in
      case ctrlC of
        True -> GameExit GameExitSuccess
        False -> status
        

moveObj :: GameObject -> (Double, Double) -> GameObject
moveObj obj@(GameObject {}) (dX, dY) = let (x, y) = objPos obj
                                       in obj { objPos = (x + dX, y + dY) }
moveObj (GameObjects x) delta = GameObjects (map (\x -> moveObj x delta) x)
