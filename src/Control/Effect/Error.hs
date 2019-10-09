{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Error
( -- * Error effect
  Error
, module Control.Effect.Throw
, module Control.Effect.Catch
) where

import Control.Effect.Catch
import Control.Effect.Sum
import Control.Effect.Throw

type Error e = Throw e :+: Catch e
