{-# LANGUAGE FlexibleContexts #-}
module State
( gen
) where

import Control.Effect.State
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (State a) sig m => Gen a -> Gen (m a)
gen a = Gen.choice [ pure get, put' <$> a, pure <$> a ] where
  put' a = a <$ put a
