module Empty
( genEmpty
) where

import Control.Effect.Empty
import Hedgehog

genEmpty :: Has Empty sig m => Gen a -> Gen (m a) -> Gen (m a)
genEmpty _ _ = pure empty
