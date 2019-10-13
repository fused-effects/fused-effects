{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Catch
( genCatchError
) where

import Control.Effect.Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen

genCatchError :: forall e m a proxy sig . (Has (Catch e) sig m, Arg e, Vary e) => proxy e -> Gen (m a)
genCatchError _ = go where
  go = fn @e go >>= subterm go . flip catchError . apply
