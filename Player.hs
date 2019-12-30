module Player where

import Card
import Control.Lens
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Monad.State.Lazy (runState)

data Player = Player
  { playerShip :: Int
  , playerHand :: [Card]
  } deriving stock (Show, Generic)

pluckPlayer :: Int -> Player -> (Card, Player)
pluckPlayer i = runState (zoom #playerHand (pluckCard' i))
