module Player where

import Card
import Data.Generics.Labels ()
import GHC.Generics (Generic)

data Player = Player
  { playerShip :: Int
  , playerHand :: [Card]
  } deriving stock (Show, Generic)
