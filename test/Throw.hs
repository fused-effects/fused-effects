module Throw
( gen
, genThrowError
) where

import Control.Effect.Throw
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Throw e) sig m => Gen e -> Gen a -> Gen (m a)
gen e a = Gen.choice [ genThrowError e, pure <$> a ]

genThrowError :: Has (Throw e) sig m => Gen e -> Gen (m a)
genThrowError e = throwError <$> e
