{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Main where

import Control.Exception (bracket)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)
import FRP.Yampa
import System.IO
  ( BufferMode (..)
  , hGetBuffering
  , hGetEcho
  , hSetBuffering
  , hSetEcho
  , hWaitForInput
  , stdin
  , stdout
  )

data Direction = North | South | West | East | Unchanged
  deriving (Show, Eq)

data Point = Point Int Int

data SnakeState = SnakeState
  { snake :: [Point]
  , direction :: Direction
  }

turnSnake :: SnakeState -> Direction -> SnakeState
turnSnake oldSnake newDirection =
  let oldDirection = direction oldSnake
      newSnake =
        if (oldDirection == North && newDirection == South)
          || (oldDirection == South && newDirection == North)
          || (oldDirection == East && newDirection == West)
          || (oldDirection == West && newDirection == East)
          then oldSnake
          else oldSnake {direction = newDirection}
   in newSnake

main :: IO ()
main = do
  timeRef <- newIORef (0 :: Int)
  let
    iSnake =
      SnakeState
        { snake = [Point 0 0]
        , direction = North
        }
    doSnake = reactimate initSnake (nextState timeRef) output (sscan turnSnake iSnake)
   in
    bracket
      (hGetEcho stdin)
      (hSetEcho stdin)
      ( \_ -> do
          hSetEcho stdin False
          bracket
            (hGetBuffering stdin)
            (hSetBuffering stdin)
            ( \_ -> do
                hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering
                hSetEcho stdout False
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

secondsSinceEpoch :: UTCTime -> Int
secondsSinceEpoch =
  floor . ((1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds)

yampaSDLTimeSense :: IORef Int -> IO DTime
yampaSDLTimeSense tr = do
  ct <- getCurrentTime
  t0 <- readIORef tr
  let
    seconds = secondsSinceEpoch ct
    n = seconds - t0
   in
    do
      writeIORef tr n
      return $ fromIntegral n

nextState :: IORef Int -> Bool -> IO (DTime, Maybe Direction)
nextState tr _ = do
  dtSecs <- yampaSDLTimeSense tr
  s <- hWaitForInput stdin 1000
  if s
    then do
      c <- getChar
      -- putStr $ show dtSecs
      let direction =
            ( case c of
                'a' -> Just West
                'w' -> Just North
                's' -> Just South
                'd' -> Just East
                _ -> Just Unchanged
            )
       in return (dtSecs, direction)
    else return (dtSecs, Nothing)

output :: Bool -> SnakeState -> IO Bool
output _ s = do
  print $ direction s
  return False