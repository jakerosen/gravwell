{-# language UndecidableInstances #-}
module RandomEffect where

import GHC.Generics (Generic1)
import Control.Algebra
import Control.Monad.IO.Class
import System.Random
import System.Random.Shuffle

import Card

data RandomEffect m k
  = RandomInt Int Int (Int -> m k)
  | ShuffleCards [Card] ([Card] -> m k)
  deriving anyclass (HFunctor, Effect)
  deriving stock (Generic1)

newtype RandomCarrier m a = RandomCarrier (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) =>
  Algebra (RandomEffect :+: sig) (RandomCarrier m) where
    alg :: (RandomEffect :+: sig) (RandomCarrier m) a -> RandomCarrier m a
    alg = \case
      L (RandomInt lo hi k) -> do
        x <- liftIO (randomRIO (lo, hi))
        k x
      L (ShuffleCards l k) -> do
        l' <- liftIO (shuffleM l)
        k l'
      R other -> RandomCarrier (alg (handleCoercible other))

randomInt :: Has RandomEffect sig m => Int -> Int -> m Int
randomInt lo hi = send (RandomInt lo hi pure)

shuffleCards :: Has RandomEffect sig m => [Card] -> m [Card]
shuffleCards l = send (ShuffleCards l pure)

runRandom :: RandomCarrier m a -> m a
runRandom (RandomCarrier m) = m
