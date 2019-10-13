{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Catch
( genCatch
) where

import Control.Effect.Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

genCatch :: forall e m a proxy sig . (Has (Catch e) sig m, Arg e, Vary e) => proxy e -> Gen (m a)
genCatch _ = go where
  go = fn @e go >>= subterm go . flip catchError . apply
