{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Catch.Internal
( Catch(..)
) where

-- | 'Catch' effects can be used alongside 'Control.Effect.Throw.Throw' to provide recoverable exceptions.
--
-- @since 1.0.0.0
data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)

deriving instance Functor m => Functor (Catch e m)
