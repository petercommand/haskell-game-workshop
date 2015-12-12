{-# LANGUAGE ExistentialQuantification #-}
module Types where

import qualified Data.Map.Lazy as M
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Data.Time.Clock
import Control.Monad.State.Lazy

class Def a where
    def :: a -- default value

instance Def UserInput where
    def = UserInput { directions = Directions False False False False
                    , mouseClick = M.empty
                    , cursor = (0,0)
                    , pressedKeys = M.empty
                    }

instance Def Config where
    def = Config { isFullscreen = False
                 , width = 600
                 , height = 600
                 }

instance Def GameObject where
    def = GameObject { objType = OtherObj
                     , objPos = (0, 0)
                     , objRot = Degree 0
                     , objScale = (0, 0)
                     , objRender = Circle 5
                     , visible = True
                     , delete = False -- mark for deletion
                     }

data ObjectType = Player | Box | OtherObj

data Config = Config { isFullscreen :: Bool
                     , width :: Int
                     , height :: Int
                     }
type ThisObject = GameObject
type OtherObject = GameObject
newtype CollisionCallback = CollisionCallback (ThisObject -> OtherObject -> ThisObject)
type Up = Bool
type Down = Bool
type Left = Bool
type Right = Bool
            
data Directions = Directions Up Down Left Right

data UserInput = UserInput { directions  :: Directions
                           , mouseClick  :: MouseClick MouseButton
                           , cursor      :: (Double, Double)
                           , pressedKeys :: PressedKeys Key
                           }

type Position = (Double, Double)
type Radius = Double
type Box = (Position, Position) -- ((Low,Low) (High, High))
data Angle = Degree Double | Radian Double

data GameState = GameState { objs :: M.Map ObjectName GameObject
                           , collisionObjs :: M.Map ObjectName CollisionOption
                           , objActions :: M.Map ObjectName ObjectAction
                           }

data ObjectAction = ObjectAction ObjectName ActionInfo (State ActionState ActionFinished)
data ActionInfo = ActionInfo { actionType :: ActionType
                             , actionDuration :: Double
                             , actionFunc :: ActionFunc
                             }
type TimeDiff = Double
data ActionFunc = ActionFunc (TimeDiff -> State ActionState ActionFinished)
data ActionState = ActionState { currentActionTime :: TimeDiff
                               }
data ActionFinished = ActionFinished | ActionNotFinished
data ActionType = ActionMove
data CollisionObj = CollisionObj GameObject CollisionOption
data CollisionOption = CollisionOption { collisionType :: CollisionType
                                       , collisionCallback :: CollisionCallback
                                       }

data GameObject = GameObject { objType :: ObjectType
                             , objPos :: (Double, Double)
                             , objRot :: Angle
                             , objScale :: (Double, Double)
                             , objRender :: Picture
                             , visible :: Bool
                             , delete :: Bool -- mark for deletion
                             }
                | GameObjects [GameObject]

data CollisionType = BoxCollision Box | CircleCollision Position Radius


class RuleFunc a where
    generateRule :: a -> Rule

data ToRuleFunc = forall a. RuleFunc a => ToRuleFunc a


data ObjectRule = ObjectRule ObjectName ObjectRuleFunc
data StatusRule = StatusRule StatusRuleFunc
data CollisionRule = CollisionRule CollisionRuleFunc

instance RuleFunc ObjectRule where
   generateRule (ObjectRule name f) =
       \(gs@(GameState { objs = objMap }), status) userInput dt -> case M.lookup name objMap of
                                                                Just ori -> let (newObj, newStatus) = f (ori, status) userInput dt
                                                                                newMap = M.adjust (const newObj) name objMap
                                                                            in (gs { objs = newMap}, newStatus)
                                                                Nothing -> ("object \"" <> name <> "\" not found in object map") `trace` (gs, status)
instance RuleFunc StatusRule where
    generateRule (StatusRule f) = \(gameState, status) userInput dt -> (gameState, f status userInput)
instance RuleFunc CollisionRule where
    generateRule (CollisionRule f) = f

type ObjectRuleFunc = (GameObject, GameStatus) -> UserInput -> NominalDiffTime -> (GameObject, GameStatus)
type StatusRuleFunc = GameStatus -> UserInput -> GameStatus
type CollisionRuleFunc = Rule
    
type Rule = (GameState, GameStatus) -> UserInput -> NominalDiffTime -> (GameState, GameStatus)


data GameStatus = GameNotStarted | GameStarted | GameEnded | GameExit GameExitReason deriving Show
data GameExitReason = GameExitSuccess | GameExitFailure String deriving Show


type PressedKeys a = M.Map a Bool
type MouseClick a = M.Map a Bool

type ObjectName = String

instance Bounded Key where
    minBound = Key'Unknown
    maxBound = Key'Menu

instance Bounded MouseButton where
    minBound = MouseButton'1
    maxBound = MouseButton'8
