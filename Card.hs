module Card where

data Card = Card
  { cardSymbol :: String
  , cardAmount :: Int
  , cardType   :: CardType
  } deriving stock ( Show )

data CardType = Fuel | Repulsor | Tractor
  deriving stock ( Show )
