{-# language UndecidableInstances #-}
module RandomEffect where

import GHC.Generics (Generic1)
import Control.Algebra
import Control.Monad.IO.Class
import System.Random

data RandomEffect m k = RandomInt Int Int (Int -> m k)
  deriving stock (Generic1)
  deriving anyclass (HFunctor, Effect)

newtype RandomCarrier m a = RandomCarrier (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) =>
  Algebra (RandomEffect :+: sig) (RandomCarrier m) where
    alg :: (RandomEffect :+: sig) (RandomCarrier m) a -> RandomCarrier m a
    alg = \case
      L (RandomInt lo hi k) -> do
        x <- liftIO (randomRIO (lo, hi))
        k x
      R other -> RandomCarrier (alg (handleCoercible other))

randomInt :: Has RandomEffect sig m => Int -> Int -> m Int
randomInt lo hi = send (RandomInt lo hi pure)

runRandom :: RandomCarrier m a -> m a
runRandom (RandomCarrier m) = m
