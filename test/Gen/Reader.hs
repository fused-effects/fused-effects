module Gen.Reader
( genReader
) where

import Control.Effect.Reader
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

genReader :: (Has (Reader a) sig m, Arg a, Vary a) => Gen a -> Gen (m a) -> Gen (m a)
genReader a ma = choice
  [ pure ask
  , fn a >>= subterm ma . local . apply
  ]
