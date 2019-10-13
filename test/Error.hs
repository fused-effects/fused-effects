module Error
( gen
) where

import Control.Effect.Error
import qualified Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified Throw

gen :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a)
gen e a = choice
  [ Throw.genThrow e
  , Catch.genCatch e (gen e a)
  , pure <$> a
  ]
