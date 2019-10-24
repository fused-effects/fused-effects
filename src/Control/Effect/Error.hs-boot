{-# LANGUAGE TypeOperators #-}
module Control.Effect.Error
( Error
) where

import Control.Algebra.Internal
import {-# SOURCE #-} Control.Effect.Catch (Catch)
import Control.Effect.Sum

type Error e = Throw e :+: Catch e
