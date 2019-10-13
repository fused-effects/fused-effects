module Empty
( gen
, genEmpty
) where

import Control.Effect.Empty
import Hedgehog
import Hedgehog.Gen

gen :: Has Empty sig m => Gen a -> Gen (m a)
gen a = choice [ genEmpty, pure <$> a ]

genEmpty :: Has Empty sig m => Gen (m a)
genEmpty = pure empty
