{-# LANGUAGE ExistentialQuantification, RankNTypes, FlexibleInstances #-}
module Types where

import qualified Data.Map.Lazy as M
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Data.Time.Clock
import Control.Monad.State.Lazy
import GHC.Float
    
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

instance Def (GameObject Picture) where
    def = GameObject { objType = OtherObj
                     , objPos = (0, 0)
                     , objRot = Degree 0
                     , objScale = (0, 0)
                     , objRender = Circle 5
                     , visible = True
                     , delete = False -- mark for deletion
                     }
instance Def (GameObject SineAnimate) where
    def = GameObject { objType = OtherObj
                     , objPos = (0, 0)
                     , objRot = Degree 0
                     , objScale = (0, 0)
                     , objRender = SineAnimate 1
                     , visible = True
                     , delete = False
                     }

          
data ObjectType = Player | Box | OtherObj deriving Show

data Config = Config { isFullscreen :: Bool
                     , width :: Int
                     , height :: Int
                     }
type ThisObject = String
type OtherObject = String
newtype CollisionCallback = CollisionCallback (ThisObject -> OtherObject -> (GameState, GameStatus) -> (GameState, GameStatus))
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

data Position = Position Double Double deriving Show
type Radius = Double
type Box = (Position, Position) -- ((Low,Low) (High, High))
data Angle = Degree Double | Radian Double deriving Show
data PolyObject = forall a. Renderable a => PolyObject (GameObject a)
                | PolyObjects [PolyObject]
data GameDebugInfo = GameDebugInfo { updateCount :: Int } deriving Show

data GameState = GameState { objs :: M.Map ObjectName PolyObject
                           , collisionObjs :: M.Map ObjectName CollisionOption
                           , objActions :: M.Map ObjectName ObjectAction
                           , diffTime :: NominalDiffTime
                           , debugInfo :: Maybe GameDebugInfo
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
data CollisionObj a = CollisionObj (GameObject a) CollisionOption
data CollisionOption = CollisionOption { collisionInfo :: CollisionType
                                       , collisionUseObjPos :: Bool
                                       , collisionCallback :: Maybe CollisionCallback
                                       }
data SineAnimate = SineAnimate { currentTime :: Double } deriving Show

instance Renderable SineAnimate where
    toPicture x = Circle $ (* 10) (sin $ (double2Float $ currentTime x))
    nextFrame x dt = let cTime = currentTime (objRender x) + realToFrac dt
                         newRender = (objRender x) { currentTime = cTime }
                     in x { objRender = newRender }
                     
class Renderable a where
    toPicture :: a -> Picture
    nextFrame :: GameObject a -> NominalDiffTime -> GameObject a

instance Renderable Picture where
    toPicture = id
    nextFrame = const

data Renderable a => GameObject a = GameObject { objType :: ObjectType
                                               , objPos :: (Double, Double)
                                               , objRot :: Angle
                                               , objScale :: (Double, Double)
                                               , objRender :: a
                                               , visible :: Bool
                                               , delete :: Bool -- mark for deletion
                                               }

                

type Width = Double
type Height = Double
data CollisionType = BoxCollision Position Width Height | CircleCollision Position Radius


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

type ObjectRuleFunc = (PolyObject, GameStatus) -> UserInput -> NominalDiffTime -> (PolyObject, GameStatus)
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
