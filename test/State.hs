{-# LANGUAGE FlexibleContexts #-}
module State
( gen
, genState
) where

import Control.Effect.State
import Hedgehog
import Hedgehog.Gen

gen :: Has (State a) sig m => Gen a -> Gen (m a)
gen a = choice [ genState a (gen a) , pure <$> a ]

genState :: Has (State a) sig m => Gen a -> Gen (m a) -> Gen (m a)
genState a _ = choice [ pure get, put' <$> a ] where
  put' a = a <$ put a
