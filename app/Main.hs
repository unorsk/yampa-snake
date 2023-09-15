{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
turnSnake oldSnake newDir =
  let oldDir = direction oldSnake
      updatedDir = if newDir == Unchanged || not (canTurn newDir oldDir) then oldDir else newDir
      newSnakeHead = newHead (head $ snake oldSnake) updatedDir
      newSnake =
        oldSnake
          { direction = updatedDir
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
  tRef <- newIORef (0 :: Int)
  bracket
    getEchoAndBuffer
    restoreEchoAndBuffer
    ( \_ -> do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        doSnake tRef
    )
 where
  doSnake tRef =
    reactimate
      initSnake
      (nextState tRef)
      printSnake
      (sscan turnSnake initialSnake)
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
  hWaitForInput stdin 1000 >>= doMakeState dtSecs
 where
  doMakeState :: DTime -> Bool -> IO (DTime, Maybe Direction)
  doMakeState dtSecs False = return (dtSecs, Nothing)
  doMakeState dtSecs True = do
    direction <- getChar <&> charToDirection
    -- direction <- getChar >>= return . charToDirection
    return (dtSecs, Just direction)
  charToDirection :: Char -> Direction
  charToDirection = \case
    'a' -> West
    'w' -> North
    's' -> South
    'd' -> East
    _ -> Unchanged

printSnake :: Bool -> SnakeState -> IO Bool
printSnake _ (SnakeState s _d) = do
  printPoint '@' $ head s
  printPoint ' ' $ last s
  putStr "\ESC[0;0H" -- go to top left corner
  return False

secondsTick :: IORef Int -> IO DTime
secondsTick tr = do
  ct <- getCurrentTime
  t0 <- readIORef tr
  let n = (t0 - floor (utcTimeToPOSIXSeconds ct))
   in do
        writeIORef tr n
        return $ fromIntegral n

goto :: Int -> Int -> IO ()
goto line col = putStr $ "\ESC[" ++ show line ++ ";" ++ show col ++ "H"

printPoint :: Char -> Point -> IO ()
printPoint c (Point x y) = do
  goto x y
  putChar c