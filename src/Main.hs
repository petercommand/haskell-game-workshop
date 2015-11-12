module Main where
import Game
import Config
import Data.List


main :: IO ()
main = do
  config <- loadConfig
  startGame config
  
