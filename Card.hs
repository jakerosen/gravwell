module Card where

data Card = Card
  { cardSymbol :: String
  , cardAmount :: Int
  , cardType   :: CardType
  } deriving stock ( Show, Eq )

data CardType = Fuel -- | Repulsor | Tractor
  deriving stock ( Show, Eq )
