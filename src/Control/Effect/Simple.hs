{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | An effect that promotes a simple higher-order function into a first-order effect, trading efficiency for simplicity.

For more information about the tradeoffs associated with this formulation of effects, please see "Control.Carrier.Simple".

Predefined carriers:

* "Control.Carrier.Simple.SimpleC"

@since 1.2.0.0
-}

module Control.Effect.Simple
( -- * Simple effect
  Simple (..)
, sendSimple
, sendSimple2
, sendSimple3
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)

data Simple e (m :: Type -> Type) a where
  Simple :: e a -> Simple e m a

-- | Invoke an effect constructor. Can be composed with 'Prelude.(.)' to wrap constructors of one argument.
sendSimple :: Has (Simple eff) sig m => eff a -> m a
sendSimple = send . Simple

-- | A convenience function for wrapping an effect constructor of two arguments.
sendSimple2 :: Has (Simple eff) sig m => (a -> b -> eff c) -> a -> b -> m c
sendSimple2 constructor arg = send . Simple . constructor arg

-- | A convenience function for wrapping an effect constructor of three arguments.
sendSimple3 :: Has (Simple eff) sig m => (a -> b -> c -> eff d) -> a -> b -> c -> m d
sendSimple3 constructor arg1 arg2 = send . Simple . constructor arg1 arg2
