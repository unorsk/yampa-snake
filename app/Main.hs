module Main where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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

newHead :: Point -> Direction -> Point
newHead (Point line col) North = Point (line - 1) col
newHead (Point line col) South = Point (line + 1) col
newHead (Point line col) West = Point line (col - 1)
newHead (Point line col) East = Point line (col + 1)
newHead p Unchanged = p

turnSnake :: SnakeState -> Direction -> SnakeState
turnSnake oldSnake newDirection =
  let oldOldDirection = direction oldSnake
      oldDirection =
        if newDirection == Unchanged || not (canTurn newDirection oldOldDirection)
          then oldOldDirection
          else newDirection
      newSnakeHead = newHead (head $ snake oldSnake) oldDirection
      newSnake =
        oldSnake
          { direction = oldDirection
          , snake = newSnakeHead : init (snake oldSnake)
          }
   in newSnake
 where
  canTurn :: Direction -> Direction -> Bool
  canTurn North South = False
  canTurn South North = False
  canTurn West East = False
  canTurn East West = False
  canTurn _ _ = True

initialSnake :: SnakeState
initialSnake =
  SnakeState
    { snake =
        [ Point 1 8
        , Point 1 7
        , Point 1 6
        , Point 1 5
        , Point 1 4
        , Point 1 3
        , Point 1 2
        , Point 1 1
        ]
    , direction = East
    }

main :: IO ()
main = do
  timeRef <- newIORef (0 :: Int)
  let
    doSnake =
      reactimate
        initSnake
        (nextState timeRef)
        outputSnake
        (sscan turnSnake initialSnake)
   in
    bracket
      getEchoAndBuffer
      restoreEchoAndBuffer
      ( \_ -> do
          hSetEcho stdin False
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          doSnake
      )
 where
  getEchoAndBuffer :: IO (Bool, BufferMode)
  getEchoAndBuffer = do
    e <- hGetEcho stdin
    b <- hGetBuffering stdin
    return (e, b)
  restoreEchoAndBuffer :: (Bool, BufferMode) -> IO ()
  restoreEchoAndBuffer (e, b) = do
    hSetEcho stdin e
    hSetBuffering stdin b

initSnake :: IO Direction
initSnake = do
  putStr "\ESC[2J" -- clear screen
  traverse_ (printPoint '@') $ snake initialSnake
  return East

nextState :: IORef Int -> Bool -> IO (DTime, Maybe Direction)
nextState tr _ = do
  dtSecs <- secondsTick tr
  s <- hWaitForInput stdin 1000
  if s
    then do
      c <- getChar
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

outputSnake :: Bool -> SnakeState -> IO Bool
outputSnake _ (SnakeState s _d) =
  let c = '@'
   in do
        printPoint c $ head s
        printPoint ' ' $ last s
        putStr "\ESC[0;0H" -- go to top left corner
        return False

secondsTick :: IORef Int -> IO DTime
secondsTick tr = do
  ct <- getCurrentTime
  t0 <- readIORef tr
  let
    seconds = secondsSinceEpoch ct
    n = seconds - t0
   in
    do
      writeIORef tr n
      return $ fromIntegral n
 where
  secondsSinceEpoch :: UTCTime -> Int
  secondsSinceEpoch =
    floor . ((1e3 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds)

goto :: Int -> Int -> IO ()
goto line col = putStr $ "\ESC[" ++ show line ++ ";" ++ show col ++ "H"

printPoint :: Char -> Point -> IO ()
printPoint c (Point x y) = do
  goto x y
  putChar c