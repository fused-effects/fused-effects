module Throw
( gen
, genThrow
) where

import Control.Effect.Throw
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Throw e) sig m => Gen e -> Gen a -> Gen (m a)
gen e a = Gen.choice [ genThrow e, pure <$> a ]

genThrow :: Has (Throw e) sig m => Gen e -> Gen (m a)
genThrow e = throwError <$> e
