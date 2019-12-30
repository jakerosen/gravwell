{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Category ((>>>))
import Control.Lens ((^.))
import Data.Foldable (foldlM)
import Data.Functor ((<&>), void)
import Data.Maybe
import qualified System.Console.ANSI as Ansi
import System.Environment (lookupEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, getStdGen)
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

import Card
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

drawShip :: Int -> Int -> (String -> String) -> IO ()
drawShip x y color = do
  Ansi.setCursorPosition x y
  putStr ( color ">" )

displayGame :: Game -> IO ()
displayGame game@Game{..} = do
  Ansi.setCursorPosition 0 0
  Ansi.clearFromCursorToScreenEnd

  if debug then
    pPrint game
  else do
    drawShip 1 (game ^. #gamePlayer2 . #playerShip) red
    drawShip 1 (game ^. #gamePlayer3 . #playerShip) red
    drawShip 1 (game ^. #gamePlayer4 . #playerShip) red
    drawShip 1 (game ^. #gamePlayer1 . #playerShip) blue
    drawShip 1 gameDerelict1 black
    drawShip 1 gameDerelict2 black

    foldlM_
      ( \n card -> do
          m <- displayCard ( 3, n ) card
          putStr ", "
          pure ( n + m + 2 )
      )
      0
      (game ^. #gamePlayer1 . #playerHand)

    Ansi.setCursorPosition 5 0
    putStrLn "Run with 'debug' environment var to see game state instead.\n"

-- | Display a card at the given (row, col) and return how many characters wide
-- it is.
displayCard :: ( Int, Int ) -> Card -> IO Int
displayCard ( row, col ) Card{..} = do
  Ansi.setCursorPosition row col
  case cardType of
    Fuel -> putStr ( green s )
    Repulsor -> putStr ( magenta s )
    Tractor -> putStr ( cyan s )
  pure ( length s )

  where
    s :: [ Char ]
    s = show cardAmount ++ " " ++ cardSymbol

blue, green, red, black, magenta, cyan :: [ Char ] -> [ Char ]
blue = style ( fg Ansi.Blue )
green = style ( fg Ansi.Green )
red = style ( fg Ansi.Red )
black = style ( fg Ansi.Black )
magenta = style ( fg Ansi.Magenta )
cyan = style ( fg Ansi.Cyan )

fg :: Ansi.Color -> Ansi.SGR
fg c = Ansi.SetColor Ansi.Foreground Ansi.Vivid c

style :: Ansi.SGR -> [ Char ] -> [ Char ]
style c s = Ansi.setSGRCode [ c ] ++ s ++ Ansi.setSGRCode [ Ansi.Reset ]

foldlM_
  :: ( Foldable t, Monad m )
  => ( b -> a -> m b )
  -> b
  -> t a
  -> m ()
foldlM_ f z xs =
  void ( foldlM f z xs )
