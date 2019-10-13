module Error
( genError
) where

import Control.Effect.Error
import qualified Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import qualified Throw

genError :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
genError e a ma = choice
  [ Throw.genThrow e a ma
  , Catch.genCatch e a ma
  ]
