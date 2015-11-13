module Types where

import qualified Data.Map.Lazy as M
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss



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

data Config = Config { isFullscreen :: Bool
                     , width :: Int
                     , height :: Int
                     }
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

data Angle = Degree Double | Radian Double


data GameObject = GameObject { objPos :: (Double, Double)
                             , objRender :: Picture
                             }
                | GameObjects [GameObject]

type RuleFunc = (GameObject, GameStatus) -> UserInput -> (GameObject, GameStatus)
type Rule = (M.Map ObjectName GameObject, GameStatus) -> UserInput -> (M.Map ObjectName GameObject, GameStatus)


data GameStatus = GameNotStarted | GameStarted | GameEnded


data GLFWKeys = GLFWModifierKey | GLFWKey deriving Enum

type PressedKeys a = M.Map a Bool
type MouseClick a = M.Map a Bool

type ObjectName = String

instance Bounded Key where
    minBound = Key'Unknown
    maxBound = Key'Menu

instance Bounded MouseButton where
    minBound = MouseButton'1
    maxBound = MouseButton'8
