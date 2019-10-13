{-# LANGUAGE FlexibleContexts #-}
module Writer
( gen
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Writer a) sig m => Gen a -> Gen (m a)
gen a = Gen.choice [ tell' <$> a, pure <$> a ] where
  tell' a = a <$ tell a
