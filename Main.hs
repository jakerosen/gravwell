{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Carrier.Writer.Strict
import Control.Category ((>>>))
import Control.Lens ((^.), (^..), _2, folded)
import Data.Foldable (for_)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (intercalate)
import Data.Maybe
import qualified System.Console.ANSI as Ansi
import System.Environment (lookupEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

import Card
import ColorStrings
import Game
import Player
import RandomEffect

-- run the game in debug mode
debug :: Bool
debug =
  unsafePerformIO do
    isJust <$> lookupEnv "debug"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  loop [] initialGame

loop :: [String] -> Game -> IO ()
loop output game = do
  validateGame game
  displayGame game output
  if gameOver game
    then putStrLn "Game over!"
    else loop' game

loop' :: Game -> IO ()
loop' game = do
  case gameState game of
    DraftBegan next -> do
      putStrLn "Press enter to draft."
      _ <- getLine
      loop ["Round " ++ show (game ^. #gameRound) ++ ":"]
        =<< (next & runRandom)

    DraftPickPlayer next -> do
      let
        again = do
          putStrLn "Pick a card."
          x <- getInt
          case next x of
            Nothing -> again
            Just action -> do
              (output, game') <- action & runRandom & runWriter
              loop output game'
      again

    DraftPickAI next -> do
      putStrLn "AI picking. Press enter to continue."
      _ <- getLine
      (output, game') <- next & runRandom & runWriter
      loop output game'

    RoundBegan next -> do
      putStrLn "Press enter to start."
      void getLine
      loop [] next

    PickCard next -> do
      let
        again = do
          putStrLn "Pick a card."
          x <- getInt
          case next x of
            Nothing -> again
            Just action -> action & runRandom >>= loop []
      again

    ResolvingMovement next -> do
      putStrLn "Press enter."
      void getLine
      (output, game') <- next & runWriter
      loop output game'

    RoundEnded next -> do
      putStrLn "Press enter to go to the next round."
      _ <- getLine
      loop [] next

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

displayGame :: Game -> [String] -> IO ()
displayGame game@Game{..} output = do
  Ansi.setCursorPosition 0 0
  Ansi.clearFromCursorToScreenEnd

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


  case gameState of
    DraftPickPlayer _ -> do
      let
        visible :: [Card]
        visible = game ^. #gameUndraftedCards & map fst

      foldlM_
        ( \n card -> do
            m <- displayCard ( 5, n ) card
            putStr ", "
            pure ( n + m + 2 )
        )
        0
        visible

    DraftPickAI _ -> do
      let
        visible :: [Card]
        visible = game ^. #gameUndraftedCards & map fst

      foldlM_
        ( \n card -> do
            m <- displayCard ( 5, n ) card
            putStr ", "
            pure ( n + m + 2 )
        )
        0
        visible

    _ ->
      foldlM_
        ( \n card -> do
            m <- displayCard ( 5, n ) card
            putStr ", "
            pure ( n + m + 2 )
        )
        0
        (game ^.. #gameUnplayedCards . folded . _2)

  Ansi.setCursorPosition 7 0

  if debug then do
    let ppDerelict n derelict = "Derelict " ++ n ++ ": Ship = " ++ show derelict
    let ppHand = intercalate ", " . map ppCard
    let ppPlayer n player =
          unwords
            [ "Player " ++ n ++ ":"
            , "Ship = " ++ show ( playerShip player ) ++ ","
            , "Hand = " ++ ppHand ( playerHand player )
            ]
    let ppState s = "State: " ++ show s
    putStrLn ( ppPlayer "1" gamePlayer1 )
    putStrLn ( ppPlayer "2" gamePlayer2 )
    putStrLn ( ppPlayer "3" gamePlayer3 )
    putStrLn ( ppPlayer "4" gamePlayer4 )
    putStrLn ( ppDerelict "1" gameDerelict1 )
    putStrLn ( ppDerelict "2" gameDerelict2 )
    putStrLn ( ppState gameState )
    putStrLn ""
  else do
    putStrLn "Run with 'debug' environment var to see debug game state.\n"

  for_ output putStrLn
  putStrLn ""


-- | Display a card at the given (row, col) and return how many characters wide
-- it is.
displayCard :: ( Int, Int ) -> Card -> IO Int
displayCard ( row, col ) card = do
  Ansi.setCursorPosition row col
  case prettyCard card of
    ( uncolored, colored ) -> do
      putStr colored
      pure ( length uncolored )

foldlM_
  :: ( Foldable t, Monad m )
  => ( b -> a -> m b )
  -> b
  -> t a
  -> m ()
foldlM_ f z xs =
  void ( foldlM f z xs )
