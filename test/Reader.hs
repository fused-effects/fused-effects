module Reader
( gen
) where

import Control.Effect.Reader
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

gen :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a)
gen a = choice
  [ pure ask
  , fn a >>= subterm (gen a) . local . apply
  , pure <$> a
  ]
