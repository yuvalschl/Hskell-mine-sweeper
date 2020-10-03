{-# OPTIONS -Wall #-}

import Lib()
import MineSweeper
import System.Random
import Data.Char()
import Data.List()
import Data.List.Split
import Safe
import System.Environment
import System.Exit
import Text.Read()


main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
   then do
    putStrLn "invalid parameters"
    exitFailure
   else
    putStrLn ""
  let _width = readMay (head args)
      _height = readMay (args !! 1)
      _minesCount = readMay (args !! 2)
      argsList = [_width, _height, _minesCount]
  if not (validateTypes argsList) && validateArgs args
    then (do let _width = read $ head args :: Int
                 _height = read $ args !! 1 :: Int
                 _minesCount = read $ args !! 2 :: Int
             gen <- getStdGen
             _ <- gameStep (newGame [] _width _height _minesCount gen)
             exitSuccess)
    else (do putStr "invalid parameters"
             exitFailure)


readAction :: Board -> IO Action
readAction board = do
  args <- getLine
  let argsList = splitOn " " args
  if length argsList /= 3
    then do putStrLn "invalid action"
            readAction board
    else do
      let action = head argsList
          _width = readMay $ argsList !! 1
          _height = readMay $ argsList !! 2
      if validateActionType _width _height
        then do
          let _row = read $ argsList !! 1 :: Int
              _col = read $ argsList !! 2 :: Int
          if validateAction action _row _col (width board) (height board)
            then return (determineAction action _row _col)
            else do putStrLn "invalid action, try again"
                    readAction board
        else do putStrLn "invalid action, try again"
                readAction board


gameStep :: Board -> IO GameState
gameStep board = do
  putStr $ showGame board Playing
  putStrLn "what is your next move?"
  action <- readAction board
  if keepPlaying (act board action)
    then gameStep (act board action)
    else
     if isBoom (matrix (act board action))
       then do
         putStr $ showGame board Lost
         putStrLn "Boom! game is over"
         return (getState (act board action))
       else do
         putStrLn $ showGame (act board action) Won
         putStrLn "you win! all mines cleared"
         return (getState (act board action))
