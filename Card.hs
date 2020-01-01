{-# LANGUAGE RecordWildCards #-}
module Card where

import Control.Algebra
import Control.Effect.State
import Data.List (delete)
import GHC.Generics (Generic)

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

-- pretty prints the colored version of a card
ppCard :: Card -> [Char]
ppCard = snd . prettyCard

-- Plucks something out of a list
pluck :: Eq a => Int -> [a] -> ([a], a)
pluck n xs =
  let
    x = xs !! n
    xs' = delete x xs
  in (xs', x)

-- State based version of pluck (which can be zoomed with zoomy)
pluck'
  :: (Eq a, Has (State [a]) sig m, Effect sig)
  => Int
  -> m a
pluck' n = do
  xs <- get
  let (xs', x) = pluck n xs
  put xs'
  pure x

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
