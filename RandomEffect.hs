{-# language UndecidableInstances #-}
module RandomEffect where

import Control.Algebra
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import System.Random
import System.Random.Shuffle

import Card

-- Gives the ability to call upon a random effect
-- Current capabilities are: asking for a random int and shuffling a
-- list of Cards
data RandomEffect m k
  = RandomInt Int Int (Int -> m k)
  | ShuffleCards [Card] ([Card] -> m k)
  deriving anyclass (HFunctor, Effect)
  deriving stock (Generic1)

-- The Carrier that can supply a random effect
newtype RandomCarrier m a = RandomCarrier (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- RandomEffect m k ::
-- Algebra sig m => Has RandomEffect sig m
-- Effect sig
instance (Algebra sig m, MonadIO m) =>
  Algebra (RandomEffect :+: sig) (RandomCarrier m) where

    -- Construct a RandomCarrier from a sum of RandomEffects and remaining
    -- effects
    alg :: (RandomEffect :+: sig) (RandomCarrier m) a -> RandomCarrier m a
    alg = \case
      -- Uses IO to call upon a random Int within this range
      L (RandomInt lo hi k) -> do
        x <- liftIO (randomRIO (lo, hi))
        k x

      -- Uses IO to shuffle this list of Cards
      L (ShuffleCards l k) -> do
        l' <- liftIO (shuffleM l)
        k l'

      -- Remaining effects are put back
      R other -> RandomCarrier (alg (handleCoercible other))

-- calls upon a random int from the sky (m provides the randomness)
randomInt :: Has RandomEffect sig m => Int -> Int -> m Int
randomInt lo hi = send (RandomInt lo hi pure)

-- Uses the randomness of m to shuffle this list of Cards
shuffleCards :: Has RandomEffect sig m => [Card] -> m [Card]
shuffleCards l = send (ShuffleCards l pure)

-- Strips the RandomCarrier newtype wrapping
runRandom :: RandomCarrier m a -> m a
runRandom (RandomCarrier m) = m
