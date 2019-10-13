module Error
( gen
, genError
) where

import Control.Effect.Error
import qualified Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified Throw

gen :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a)
gen e a = choice
  [ genError e a (gen e a)
  , pure <$> a
  ]

genError :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
genError e a ma = choice
  [ Throw.genThrow e a ma
  , Catch.genCatch e a ma
  ]
