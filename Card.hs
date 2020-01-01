{-# LANGUAGE RecordWildCards #-}
module Card where

import Data.List (delete)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import GHC.Generics (Generic)
-- import qualified Control.Carrier.State.Strict as FE
import qualified Control.Effect.State as FE
import qualified Control.Algebra as FE

import ColorStrings

data Card = Card
  { cardSymbol :: String
  , cardAmount :: Int
  , cardType   :: CardType
  } deriving stock ( Show, Eq, Generic )

data CardType = Fuel | Repulsor | Tractor
  deriving stock ( Show, Eq )

-- | Returns (uncolored, colored)
prettyCard :: Card -> ( [ Char ], [ Char ] )
prettyCard Card{..} =
  ( s
  , case cardType of
      Fuel -> green s
      Repulsor -> magenta s
      Tractor -> cyan s
  )
  where
    s :: [ Char ]
    s = show cardAmount ++ " " ++ cardSymbol

ppCard :: Card -> [Char]
ppCard = snd . prettyCard

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
  , Card "Dy" 5 Fuel
  , Card "Es" 2 Fuel
  , Card "F" 6 Fuel
  , Card "Ga" 5 Fuel
  , Card "H" 4 Fuel
  , Card "I" 6 Fuel
  , Card "Jo" 2 Tractor
  , Card "Kr" 2 Repulsor
  , Card "Li" 4 Fuel
  , Card "Mg" 10 Fuel
  , Card "Ne" 6 Repulsor
  , Card "O" 7 Fuel
  , Card "Pu" 5 Fuel
  , Card "Qt" 3 Tractor
  , Card "Ra" 9 Fuel
  , Card "Si" 9 Fuel
  , Card "Th" 2 Fuel
  , Card "U" 5 Repulsor
  , Card "V" 7 Fuel
  , Card "W" 8 Fuel
  , Card "Xe" 3 Repulsor
  , Card "Y" 8 Fuel
  , Card "Zr" 7 Fuel
  ]
