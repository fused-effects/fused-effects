module Throw
( gen
) where

import Control.Effect.Throw
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Throw e) sig m => Gen e -> Gen a -> Gen (m a)
gen e a = Gen.choice [ throwError <$> e, pure <$> a ]
