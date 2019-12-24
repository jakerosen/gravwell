module Card where

data Card = Card
  { cardSymbol :: String
  , cardAmount :: Int
  , cardType   :: CardType
  }

data CardType = Fuel | Repulsor | Tractor
