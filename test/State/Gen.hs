{-# LANGUAGE FlexibleContexts #-}
module State.Gen
( genState
) where

import Control.Effect.State
import Hedgehog
import Hedgehog.Gen

genState :: Has (State a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genState a _ = choice [ pure get, put' <$> a ] where
  put' a = a <$ put a
