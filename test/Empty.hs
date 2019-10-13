module Empty
( gen
) where

import Control.Effect.Empty
import Hedgehog
import Hedgehog.Gen

gen :: Has Empty sig m => Gen a -> Gen (m a)
gen a = choice [ pure empty, pure <$> a ]
