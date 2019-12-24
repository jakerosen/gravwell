module Main where

import Control.Category ((>>>))
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Functor ((<&>))
import Text.Read (readMaybe)

import Game


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
    RoundBegun next -> do
      putStrLn "Pick a card."

      fix \again ->
        ( getInt <&> next ) >>=
          maybe again loop

getInt :: IO Int
getInt =
  getLine >>=
    ( readMaybe >>> maybe getInt pure )

displayGame = print
displayOutput = print @()
gameOver = const False
