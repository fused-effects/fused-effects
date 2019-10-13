module Throw
( genThrow
) where

import Control.Effect.Throw
import Hedgehog

genThrow :: Has (Throw e) sig m => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
genThrow e _ _ = throwError <$> e
