module Throw
( genThrow
) where

import Control.Effect.Throw
import Hedgehog

genThrow :: Has (Throw e) sig m => Gen e -> Gen (m a)
genThrow e = throwError <$> e
