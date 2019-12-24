module Game where

import Card

data Game = Game
  { gameHand :: [Card]
  , gameShip :: Int
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameState :: GameState
  }

data GameState =
    RoundBegun (Int -> Maybe Game)
