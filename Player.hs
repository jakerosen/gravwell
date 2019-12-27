module Player where

import GHC.Generics (Generic)
import Card

data Player = Player
  { playerShip :: Int
  , playerHand :: [Card]
  } deriving stock (Show, Generic)
