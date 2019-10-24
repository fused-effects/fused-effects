{-# LANGUAGE TypeOperators #-}
module Control.Effect.Error
( Error
) where

import Control.Algebra.Internal (Catch, Throw)
import Control.Effect.Sum

type Error e = Throw e :+: Catch e
