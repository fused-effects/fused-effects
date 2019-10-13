module Reader
( gen
) where

import Control.Effect.Reader
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen as Gen

gen :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a)
gen a = Gen.choice
  [ pure ask
  , fn a >>= \ f -> subterm (gen a) (local (apply f))
  , pure <$> a
  ]
