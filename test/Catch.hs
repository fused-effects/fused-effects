{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Catch
( gen
) where

import Control.Effect.Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

gen :: forall e m a sig . (Has (Catch e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
gen _ _ ma = choice [ fn @e ma >>= subterm ma . flip catchError . apply ]
