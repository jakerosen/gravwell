module Card where

import Data.List (delete)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import GHC.Generics (Generic)
-- import qualified Control.Carrier.State.Strict as FE
import qualified Control.Effect.State as FE
import qualified Control.Algebra as FE

data Card = Card
  { cardSymbol :: String
  , cardAmount :: Int
  , cardType   :: CardType
  } deriving stock ( Show, Eq, Generic )

data CardType = Fuel | Repulsor | Tractor
  deriving stock ( Show, Eq )

pluckCard :: Int -> [Card] -> (Card, [Card])
pluckCard i cards =
  let
    card = cards !! i
    cards' = delete card cards
  in (card, cards')

pluckCardFE :: Int -> [Card] -> ([Card], Card)
pluckCardFE i cards =
  let
    card = cards !! i
    cards' = delete card cards
  in (cards', card)

pluckCard' :: Int -> State [Card] Card
pluckCard' n = State.state (pluckCard n)

-- pluckCard'FE
--   :: Applicative m--FE.Has (FE.State [Card]) sig m
--   => Int
--   -> FE.StateC [Card] m Card
-- pluckCard'FE n = FE.StateC (pure <$> pluckCardFE n)

pluckCard'FE
  :: (FE.Has (FE.State [Card]) sig m, FE.Effect sig)
  => Int
  -> m Card
pluckCard'FE n = do
  cards <- FE.get
  let (cards', card) = pluckCardFE n cards
  FE.put cards'
  pure card

deck :: [Card]
deck =
  [ Card "A" 1 Fuel
  , Card "B" 2 Fuel
  , Card "C" 3 Fuel
  , Card "D" 4 Fuel
  , Card "E" 5 Fuel
  , Card "F" 6 Fuel
  , Card "G" 7 Fuel
  , Card "H" 8 Fuel
  , Card "I" 9 Fuel
  , Card "J" 10 Fuel
  , Card "K" 11 Fuel
  , Card "L" 12 Fuel
  , Card "M" 13 Fuel
  , Card "N" 14 Fuel
  , Card "O" 15 Fuel
  , Card "P" 16 Fuel
  , Card "Q" 17 Fuel
  , Card "R" 18 Fuel
  , Card "S" 19 Fuel
  , Card "T" 20 Fuel
  , Card "U" 21 Fuel
  , Card "V" 22 Fuel
  , Card "W" 23 Fuel
  , Card "X" 24 Fuel
  , Card "Y" 25 Fuel
  , Card "Z" 26 Fuel
  ]
