module Game where

import Data.List (sort)

import Card


data Game = Game
  { gameHand :: [Card]
  , gameShip :: Int
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameState :: GameState
  } deriving stock (Show)

data GameState =
    RoundBegun (Int -> Maybe Game)

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
