module Empty
( gen
, genEmpty
) where

import Control.Effect.Empty
import Hedgehog
import Hedgehog.Gen

gen :: Has Empty sig m => Gen a -> Gen (m a)
gen a = choice [ genEmpty a (gen a), pure <$> a ]

genEmpty :: Has Empty sig m => Gen a -> Gen (m a) -> Gen (m a)
genEmpty _ _ = pure empty
