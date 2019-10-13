module Reader
( gen
) where

import Control.Effect.Reader
import Hedgehog
import Hedgehog.Gen as Gen

gen :: Has (Reader a) sig m => Gen a -> Gen (m a)
gen a = Gen.choice [ pure ask, pure <$> a ]
