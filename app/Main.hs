module Main where

import FRP.Yampa

data Direction = North | South | West | East
data Point = Point Int Int

data SnakeState = SnakeState
  { snake :: [Point]
  , direction :: Direction
  }

signalFunction :: SF Double Double
signalFunction = (/) <$> integral <*> time

main :: IO ()
main =
  reactimate initSnake nextState output signalFunction

initSnake :: IO SnakeState
initSnake =
  return $
    SnakeState
      { snake = [Point 0 0]
      , direction = North
      }

nextState :: Bool -> IO (DTime, Maybe SnakeState)
nextState _ = return (1.0, Nothing)

output :: Bool -> SnakeState -> IO Bool
output _ _ = return False
