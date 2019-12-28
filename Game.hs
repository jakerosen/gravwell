{-# language RecordWildCards #-}
module Game where

-- import Data.List (sort)

-- import Debug.Trace
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Player
import Control.Lens
import Card
import System.Random (StdGen)
import qualified System.Random as Random
import System.Random.Shuffle (shuffle')
import Control.Monad
import Data.List
-- import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


data Game = Game
  { gamePlayer1 :: Player
  , gamePlayer2 :: Player
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameState :: GameState
  , gameRandom :: StdGen
  } deriving stock (Show, Generic)

instance Show GameState where
  show = \case
    RoundBegan{} -> "RoundBegan"
    DraftBegan{} -> "DraftBegan"
    RoundEnded{} -> "RoundEnded"

-- | Returns 1 (forwards), -1 (backwards), or 0 (nowhere) per the natural motion
-- direction of the given index in the game.
gameMotion :: Game -> Int -> Int
gameMotion game i =
  case comp1 <> comp2 of
    LT -> -1
    EQ -> 0
    GT -> 1
  where
    comp1 = compare closestShipBehind closestShipAhead
    comp2 = compare numShipsAhead numShipsBehind

    (shipsBehind, shipsAhead) = Set.split i ships

    closestShipBehind = case (Set.lookupMax shipsBehind) of
      Nothing -> maxBound
      Just n -> i - n

    closestShipAhead = case (Set.lookupMin shipsAhead) of
      Nothing -> maxBound
      Just n ->  n - i

    numShipsBehind = length shipsBehind
    numShipsAhead = length shipsAhead

    ships :: Set Int
    ships = gameShips game

gamePlayers :: Game -> [Player]
gamePlayers game =
  [ gamePlayer1 game
  , gamePlayer2 game
  ]

gameOver :: Game -> Bool
gameOver game = any (>= gameWarpGate) (gameShipsList game)

gameWarpGate :: Int
gameWarpGate = 30

-- | Positions of all ships in the game
gameShips :: Game -> Set Int
gameShips = Set.fromList . gameShipsList

gameShipsList :: Game -> [Int]
gameShipsList game =
  [ gameDerelict1 game
  , gameDerelict2 game
  ] ++ map (view #playerShip) (gamePlayers game)

data GameState =
    RoundBegan (Int -> Maybe Game)
  | DraftBegan Game
  | RoundEnded Game

-- The initial game state
initialGame :: StdGen -> Game
initialGame random = setStateDraftBegan
  Game
    { gamePlayer1 = Player 0 []
    , gamePlayer2 = Player 0 []
    , gameDerelict1 = 10
    , gameDerelict2 = 20
    , gameState = undefined -- intentionally undefined, will be set later
    , gameRandom = random
    }

-- Sets the game state of this Game to RoundBegan
setStateRoundBegan :: Game -> Game
setStateRoundBegan game0 = game1
  where
    f :: Int -> Maybe Game
    f x = if x >= length (game1 ^. #gamePlayer1 . #playerHand)
      then Nothing
      else Just $ handlePlayCard x game1

    game1 :: Game
    game1 = game0 { gameState = RoundBegan f }

-- Sets the game state of this Game to DraftBegan
setStateDraftBegan :: Game -> Game
setStateDraftBegan game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = DraftBegan game2 }

    game2 :: Game
    game2 = game1
      & #gamePlayer1 . #playerHand .~ draftHand1
      & #gamePlayer2 . #playerHand .~ draftHand2
      & #gameRandom .~ ran2
      & setStateRoundBegan

    (ran1, ran2) = Random.split (gameRandom game1)

    -- draftHand :: [Card]
    -- draftHand = take 6 deck'

    deck' :: [Card]
    deck' = shuffle' deck 26 ran1

    (draftHand1, (draftHand2, _))
      =             (splitAt 6 deck')
      & _2 %~       (splitAt 6)
      & _2._2 %~    (splitAt 6)
      & _2._2._2 %~ (splitAt 6)

-- Sets the game state of this Game to RoundEnded
setStateRoundEnded :: Game -> Game
setStateRoundEnded game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = RoundEnded game2 }

    game2 :: Game
    game2 = setStateDraftBegan game1

-- Play the Card at this index and determine the resulting game
-- This must be a valid index
handlePlayCard :: Int -> Game -> Game
handlePlayCard x game0 =
  let
    handSize :: Int
    handSize = length (game0 ^. #gamePlayer1 . #playerHand)

  in if x < 0 || x >= handSize then error "Invalid card index" else
    let (card :: Card, game1 :: Game) = pluck x game0
    in if handSize == 1
      then setStateRoundEnded $ playCard card game1
      else setStateRoundBegan $ playCard card game1

-- Pluck a card at this index out of the player's hand.
-- This must be a valid index
pluck :: Int -> Game -> (Card, Game)
pluck i game =
  let
    card = game ^?! #gamePlayer1 . #playerHand . ix i
    game' = game & #gamePlayer1 . #playerHand %~ delete card
  in (card, game')

-- Play this Card and determine the resulting Game
playCard :: Card -> Game -> Game
playCard card game = case cardType card of
  Fuel ->
    let
      motion :: Int
      motion = gameMotion game (game ^. #gamePlayer1 . #playerShip)

      move :: Int
      move = motion * cardAmount card

      occupiedSpaces :: Set Int
      occupiedSpaces = gameShips game

      openPos :: Int -> Int
      openPos pos = if pos `Set.member` occupiedSpaces
        then openPos (pos + motion)
        else pos

      candidatePos :: Int
      candidatePos = game ^. #gamePlayer1 . #playerShip + move

      newPosition :: Int
      newPosition = max 0 $ if motion == 0
        then candidatePos
        else openPos candidatePos

    in game & #gamePlayer1 . #playerShip .~ newPosition
  -- Repulsor -> undefined
  -- Tractor -> undefined

validateGame :: Game -> IO ()
validateGame game@(Game { .. })  = do
  when
    -- ((gamePlayers game) & undefined)
    (any (\player -> length (playerHand player) > 6) (gamePlayers game))
    (die "Hand exceeded 6 cards")

  when (any (<0) (gameShips game)) (die "Ship index cannot be negative")

  when
    (length (filter (>=gameWarpGate) (gameShipsList game)) > 1)
    (die "Two ships cannot be in the warp gate at once")

  do
    let
      xs :: [Int]
      xs = filter (/=0) (gameShipsList game)

    when (length xs /= length (nub xs))
      (die "Ships cannot be on top of each other outside of singularity")

  where
    die :: [Char] -> IO ()
    die s = do
      print game
      fail s
