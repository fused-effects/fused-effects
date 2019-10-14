module Choose
( genChoose
) where

import Control.Effect.Choose
import Hedgehog
import Hedgehog.Gen

genChoose :: Has Choose sig m => Gen a -> Gen (m a) -> Gen (m a)
genChoose _ m = subterm2 m m (<|>)
