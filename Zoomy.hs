module Zoomy where

import Control.Lens
import Control.Carrier.State.Strict

zoomy
  :: forall s a sig m x. Has (State s) sig m
  => Lens' s a
  -> StateC a m x
  -> m x
zoomy l action = do
  s <- get
  let a = s ^. l
  (a', x) <- runState a action
  put (s & l .~ a')
  pure x
