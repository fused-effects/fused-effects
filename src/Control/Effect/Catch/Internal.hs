{-# LANGUAGE GADTs #-}
module Control.Effect.Catch.Internal
( Catch(..)
) where

-- | 'Catch' effects can be used alongside 'Control.Effect.Throw.Throw' to provide recoverable exceptions.
--
-- @since 1.0.0.0
data Catch e m k where
  Catch :: m a -> (e -> m a) -> Catch e m a
