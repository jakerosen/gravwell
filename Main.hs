module Main where

import Data.Foldable (for_)


main :: IO ()
main = do
  putStrLn "Gravwell"

  loop initialGame

loop :: Game -> IO ()
loop game = do
  displayGame game
  if gameOver game
    then putStrLn "Game over!"
    else loop' game

loop' :: Game -> IO ()
loop' game = do
  case gameState game of
    (game', outputs) -> do
      for_ outputs displayOutput
      _ <- getLine
      loop game'

type Game = ()
initialGame = ()
displayGame = print
displayOutput = print @()
gameOver = const False
gameState = const ((), [()])
