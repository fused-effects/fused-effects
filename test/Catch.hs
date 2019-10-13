{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Catch
( genCatch
) where

import Control.Effect.Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

genCatch :: forall e m a sig . (Has (Catch e) sig m, Arg e, Vary e) => Gen e -> Gen (m a) -> Gen (m a)
genCatch _ ma = choice [ fn @e ma >>= subterm ma . flip catchError . apply ]
