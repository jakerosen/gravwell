module Main where

import System.Random (StdGen, getStdGen)
import Control.Category ((>>>))
import Data.Functor ((<&>))
import Data.Maybe
import System.Environment (lookupEnv)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)
import qualified System.Console.ANSI as Ansi

import Game

debug :: Bool
debug =
  unsafePerformIO do
    isJust <$> lookupEnv "debug"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  random :: StdGen <- getStdGen
  loop (initialGame random)

loop :: Game -> IO ()
loop game = do
  validateGame game
  displayGame game
  if gameOver game
    then putStrLn "Game over!"
    else loop' game

loop' :: Game -> IO ()
loop' game = do
  case gameState game of
    RoundBegan next -> do
      putStrLn "Pick a card."

      untilJust ( getInt <&> next ) >>= loop

    DraftBegan next -> do
      putStrLn "Press enter to draft."
      _ <- getLine
      loop next
    RoundEnded next -> do
      putStrLn "Press enter to go to the next round."
      _ <- getLine
      loop next

untilJust :: IO ( Maybe a ) -> IO a
untilJust action =
  action >>= maybe ( untilJust action ) pure

getInt :: IO Int
getInt =
  getLine >>=
    ( readMaybe >>> maybe getInt pure )

displayGame :: Game -> IO ()
displayGame game = do
  Ansi.setCursorPosition 0 0
  Ansi.clearFromCursorToScreenEnd

  if debug then
    pPrint game
  else do
    Ansi.setCursorPosition 1 ( gameShip game ) >> putStr ( blue ">" )
    Ansi.setCursorPosition 1 ( gameDerelict1 game ) >> putStr ( red ">" )
    Ansi.setCursorPosition 1 ( gameDerelict2 game ) >> putStr ( red ">" )

    Ansi.setCursorPosition 3 0
    putStrLn "Run with 'debug' environment var to see game state instead.\n"

  where
    blue = style ( fg Ansi.Blue )
    red = style ( fg Ansi.Red )
    fg c = Ansi.SetColor Ansi.Foreground Ansi.Vivid c
    style c s = Ansi.setSGRCode [ c ] ++ s ++ Ansi.setSGRCode [ Ansi.Reset ]

gameOver :: Game -> Bool
gameOver game = gameShip game >= 30
