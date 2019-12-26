module Game where

-- import Data.List (sort)

import Debug.Trace
import Card
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


data Game = Game
  { gameHand :: [Card]
  , gameShip :: Int
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameState :: GameState
  } deriving stock (Show)

instance Show GameState where
  show = \case
    RoundBegan{} -> "RoundBegan"
    DraftBegan{} -> "DraftBegan"
    RoundEnded{} -> "RoundEnded"

-- | Returns 1 (forwards), -1 (backwards), or 0 (nowhere) per the natural motion
-- direction of the given index in the game.
gameMotion :: Game -> Int -> Int
gameMotion game index =
  case comp1 <> comp2 of
    LT -> -1
    EQ -> 0
    GT -> 1
  where
    comp1 = compare closestShipAhead closestShipBehind
    comp2 = compare numShipsBehind numShipsAhead

    (shipsBehind, shipsAhead) = Set.split index ships

    closestShipBehind = fromMaybe minBound (Set.lookupMax shipsBehind)
    closestShipAhead = fromMaybe maxBound (Set.lookupMin shipsAhead)
    numShipsBehind = length shipsBehind
    numShipsAhead = length shipsAhead

    ships :: Set Int
    ships = gameShips game

-- | Positions of all ships in the game
gameShips :: Game -> Set Int
gameShips game =
  Set.fromList
    [ gameShip game
    , gameDerelict1 game
    , gameDerelict2 game
    ]
data GameState =
    RoundBegan (Int -> Maybe Game)
  | DraftBegan Game
  | RoundEnded Game

-- The initial game state
initialGame :: Game
initialGame = setStateDraftBegan
  Game
    { gameHand = [ Card "A" 1 Fuel
                 , Card "B" 2 Fuel
                 , Card "C" 3 Fuel
                 , Card "D" 4 Fuel
                 , Card "E" 5 Fuel
                 , Card "F" 6 Fuel
                 ]
    , gameShip = 0
    , gameDerelict1 = 10
    , gameDerelict2 = 20
    , gameState = undefined -- intentionally undefined, will be set later
    }

-- Sets the game state of this Game to RoundBegan
setStateRoundBegan :: Game -> Game
setStateRoundBegan game0 = game1
  where
    f :: Int -> Maybe Game
    f x = Just $ handlePlayCard x game1

    game1 :: Game
    game1 = game0 { gameState = RoundBegan f }

-- Sets the game state of this Game to DraftBegan
setStateDraftBegan :: Game -> Game
setStateDraftBegan game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = DraftBegan game2 }

    game2 :: Game
    game2 = setStateRoundBegan game1
      { gameHand =
        [ Card "A" 1 Fuel
        , Card "B" 2 Fuel
        , Card "C" 3 Fuel
        , Card "D" 4 Fuel
        , Card "E" 5 Fuel
        , Card "F" 6 Fuel
        ] }

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
    handSize = length (gameHand game0)
  in if x < 0 || x >= handSize then error "Invalid card index" else
    let
      (card :: Card, game1 :: Game) = pluck x game0
      game2 = if handSize == 1
        then playCard card (setStateRoundEnded game1)
        else playCard card (setStateRoundBegan game1)
    in game2

-- Pluck a card at this index out of the player's hand.
-- This must be a valid index
pluck :: Int -> Game -> (Card, Game)
pluck index game =
  let
    card = gameHand game !! index
    game' = game { gameHand = delete card (gameHand game) }
  in (card, game')

-- Play this Card and determine the resulting Game
playCard :: Card -> Game -> Game
playCard card game = case cardType card of
  Fuel ->
    let
      motion :: Int
      motion = gameMotion game (gameShip game)

      move :: Int
      move = motion * cardAmount card

      occupiedSpaces :: Set Int
      occupiedSpaces = gameShips game

      openPos :: Int -> Int
      openPos pos = if pos `Set.member` occupiedSpaces
        then openPos (pos + motion)
        else pos

      -- TODO playCard seems to work incorrectly
      newPosition :: Int
      newPosition = max 0 $ if motion == 0
        then gameShip game + move
        else openPos (gameShip game + move)

      game' = game { gameShip = newPosition }
    in trace (show motion) game'
  -- Repulsor -> undefined
  -- Tractor -> undefined
