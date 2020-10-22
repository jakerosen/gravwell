{-# language UndecidableInstances #-}
module RandomEffect where

import Control.Algebra
import Control.Monad.IO.Class
import System.Random
import System.Random.Shuffle
import Data.Kind

import Card

-- Gives the ability to call upon a random effect
-- Current capabilities are: asking for a random int and shuffling a
-- list of Cards
data RandomEffect :: (Type -> Type) -> Type -> Type where
  RandomInt :: Int -> Int -> RandomEffect m Int
  ShuffleCards :: [Card] -> RandomEffect m [Card]

-- The Carrier that can supply a random effect
newtype RandomCarrier m a = RandomCarrier (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) =>
  Algebra (RandomEffect :+: sig) (RandomCarrier m) where
    -- Construct a RandomCarrier from a sum of RandomEffects and remaining
    -- effects
    alg
      :: Functor ctx
      => Handler ctx n (RandomCarrier m)
      -> (:+:) RandomEffect sig n a
      -> ctx ()
      -> RandomCarrier m (ctx a)
    alg hdl sig ctx = RandomCarrier $ case sig of
      -- Uses IO to call upon a random Int within this range from the sky
      L (RandomInt lo hi) -> do
        x <- liftIO (randomRIO (lo, hi))
        pure (x <$ ctx)

      -- Uses IO to shuffle this list of Cards
      L (ShuffleCards l) -> do
        l' <- liftIO (shuffleM l)
        pure (l' <$ ctx)

      -- I have an incomplete understanding of this, but I believe the idea
      -- is that "for some other computation that does not match RandomEffect,
      -- call alg to handle its operations and put it back inside of a
      -- RandomCarrier with runRandom at the end of its handler queue."
      -- Something like that, anyway
      R other -> alg (runRandom . hdl) other ctx

-- calls upon a random int from the sky (m provides the randomness)
randomInt :: Has RandomEffect sig m => Int -> Int -> m Int
randomInt lo hi = send (RandomInt lo hi)

-- Uses the randomness of m to shuffle this list of Cards
shuffleCards :: Has RandomEffect sig m => [Card] -> m [Card]
shuffleCards l = send (ShuffleCards l)

-- Strips the RandomCarrier newtype wrapping
runRandom :: RandomCarrier m a -> m a
runRandom (RandomCarrier m) = m
