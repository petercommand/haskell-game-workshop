{-# LANGUAGE ScopedTypeVariables #-}
module Input ( getUserInput
             , ctrlPressed
             , altPressed
             , shiftPressed
             , keyPressed
             ) where

import qualified Data.Map.Lazy as M
import Types
import "GLFW-b" Graphics.UI.GLFW as GLFW

getUserInput :: Window -> (UserInput -> IO a) -> IO a
getUserInput win userInputSink = 
    let
        getMapping :: (Bounded a, Ord a, Enum a) => (Window -> a -> IO Bool) -> Window -> IO (M.Map a Bool) 
        getMapping action win = M.fromList <$> (sequence $ map (\x -> action win x >>= \result -> return (x, result)) [minBound..maxBound])
    in do
      pollEvents
      keyPressMapping <- getMapping keyIsPressed win
      mouseClickMapping <- getMapping mouseButtonIsClicked win
      [u, d, l, r] <- sequence $ map (keyIsPressed win) [Key'Up, Key'Down, Key'Left, Key'Right]
      cur <- getCursorPos win
      userInputSink $ UserInput { directions = Directions u d l r
                                , mouseClick = mouseClickMapping
                                , cursor = cur
                                , pressedKeys = keyPressMapping 
                                }




keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPressed <$> GLFW.getKey win key

mouseButtonIsClicked :: Window -> MouseButton -> IO Bool
mouseButtonIsClicked win key = isClicked <$> GLFW.getMouseButton win key


isPressed :: KeyState -> Bool
isPressed KeyState'Pressed = True
isPressed KeyState'Repeating = True
isPressed _ = False

isClicked :: MouseButtonState -> Bool
isClicked MouseButtonState'Pressed = True
isClicked MouseButtonState'Released = False
              
ctrlPressed :: PressedKeys Key -> Bool
ctrlPressed keyMap = case M.lookup Key'LeftControl keyMap of
                       Just True -> True
                       _         -> case M.lookup Key'RightControl keyMap of
                                      Just True -> True
                                      _         -> False

altPressed :: PressedKeys Key -> Bool
altPressed keyMap = case M.lookup Key'LeftAlt keyMap of
                      Just True -> True
                      _         -> case M.lookup Key'RightAlt keyMap of
                                     Just True -> True
                                     _         -> False

shiftPressed :: PressedKeys Key -> Bool
shiftPressed keyMap = case M.lookup Key'LeftShift keyMap of
                        Just True -> True
                        _         -> case M.lookup Key'RightAlt keyMap of
                                       Just True -> True
                                       _         -> False

keyPressed :: PressedKeys Key -> Key -> Bool
keyPressed keyMap key = case M.lookup key keyMap of
                          Just True -> True
                          _         -> False
