{-# LANGUAGE Arrows #-}

module Main where

import Control.Exception (bracket)
import FRP.Yampa
import System.IO
  ( BufferMode (..)
  , hGetBuffering
  , hGetEcho
  , hSetBuffering
  , hSetEcho
  , stdout
  )

data Direction = North | South | West | East
data Point = Point Int Int

data SnakeState = SnakeState
  { snake :: [Point]
  , direction :: Direction
  }

signalFunction :: SF Direction SnakeState
signalFunction = undefined -- (/) <$> integral <*> time

signalFunction1 :: SF (SnakeState, Direction) SnakeState
signalFunction1 = proc (direction, snakeState) -> do
  _t <- time -< ()
  let newDirection = direction
      _newState = snakeState
  returnA -< newDirection

main :: IO ()
main =
  let doSnake = reactimate initSnake nextState output signalFunction1
   in bracket
        (hGetEcho stdout)
        (hSetEcho stdout)
        ( \_ -> do
            hSetEcho stdout False
            bracket
              (hGetBuffering stdout)
              (hSetBuffering stdout)
              ( \_ -> do
                  hSetBuffering stdout NoBuffering
                  doSnake
              )
        )

initSnake :: IO Direction
initSnake = do
  print "Initialized !"
  let _isnake =
        SnakeState
          { snake = [Point 0 0]
          , direction = North
          }
   in return West

nextState :: Bool -> IO (DTime, Maybe Direction)
nextState _ = do
  c <- getChar
  let direction =
        ( case c of
            'a' -> Just West
            'w' -> Just North
            's' -> Just South
            'd' -> Just East
            _ -> Nothing
        )
   in -- s <- initSnake
      return (1.0, direction)

output :: Bool -> SnakeState -> IO Bool
output _ _ = do
  print "Hello!"
  return False
