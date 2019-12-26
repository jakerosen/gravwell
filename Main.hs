module Main where

import Control.Category ((>>>))
-- import Data.Foldable (for_)
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
    RoundBegan next -> do
      putStrLn "Pick a card."

      fix \again ->
        ( getInt <&> next ) >>=
          maybe again loop
    DraftBegan next -> do
      putStrLn "Press enter to draft."
      _ <- getLine
      loop next
    RoundEnded next -> do
      putStrLn "Press enter to go to the next round."
      _ <- getLine
      loop next


getInt :: IO Int
getInt =
  getLine >>=
    ( readMaybe >>> maybe getInt pure )

displayGame :: Game -> IO ()
displayGame = print


-- displayOutput = print @()

gameOver :: b -> Bool
gameOver = const False
