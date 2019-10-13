{-# LANGUAGE FlexibleContexts #-}
module Writer
( gen
, genWriter
) where

import Control.Effect.Writer
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Writer a) sig m => Gen a -> Gen (m a)
gen a = Gen.choice [ genWriter a, pure <$> a ]

genWriter :: Has (Writer a) sig m => Gen a -> Gen (m a)
genWriter a = tell' <$> a where
  tell' a = a <$ tell a
