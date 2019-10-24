{-# LANGUAGE TypeOperators #-}
module Control.Effect.Error
( Error
) where

import Control.Algebra.Internal (Catch)
import Control.Effect.Sum
import {-# SOURCE #-} Control.Effect.Throw (Throw)

type Error e = Throw e :+: Catch e
