module Game where

import Data.List (sort)

import Card
import Data.List
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
    RoundBegun{} -> "RoundBegun"

-- | Returns 1 (forwards), -1 (backwards), or 0 (nowhere) per the natural motion
-- direction of the given index in the game.
gameMotion :: Game -> Int -> Int
gameMotion game index =
  case comp1 <> comp2 of
    LT -> -1
    EQ -> 0
    GT -> 1
  where
    comp1 = compare closestShipBehind (closestShipAhead :: Int)
    comp2 = compare numShipsBehind (numShipsAhead :: Int)

    closestShipBehind = undefined
    closestShipAhead = undefined
    numShipsBehind = undefined
    numShipsAhead = undefined

    ships = gameShips game

-- | Positions of all ships in the game, in ascending order.
gameShips :: Game -> [Int]
gameShips game =
  sort
    [ gameShip game
    , gameDerelict1 game
    , gameDerelict2 game
    ]
data GameState = RoundBegun (Int -> Maybe Game)

initialGame :: Game
initialGame =
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
    , gameState = undefined
    }

setStateRoundBegun :: Game -> Game
setStateRoundBegun game0 = game1
  where
    f :: Int -> Maybe Game
    f x = Just $ handlePlayCard x game1

    game1 :: Game
    game1 = game0 { gameState = RoundBegun f }

handlePlayCard :: Int -> Game -> Game
handlePlayCard x game0 =
  let
    len :: Int
    len = length (gameHand game0)
  in
    if x < 0 || x >= len then error "Invalid card index"
      else let
          (card :: Card, game1 :: Game) = pluck x game0
          game2 = undefined
        in game2

pluck :: Int -> Game -> (Card, Game)
pluck x game =
  let
    card = gameHand game !! x
    game = game { gameHand = delete card (gameHand game) }
  in (card, game)

playCard :: Card -> Game -> Game
playCard card game = case cardType card of
  Fuel ->
    let
      motion :: Int
      motion = undefined -- gameMotion

      move :: Int
      move = motion * cardAmount card

      occupiedSpaces :: Set Int
      occupiedSpaces = Set.fromList undefined -- gameShips

      openPos :: Int -> Int
      openPos pos = if pos `Set.member` occupiedSpaces
        then openPos (pos + motion)
        else pos

      newPosition :: Int
      newPosition = if motion == 0
        then gameShip game + move
        else openPos (gameShip game + move)

      game' = game { gameShip = newPosition }
    in game'
  -- Repulsor -> undefined
  -- Tractor -> undefined
