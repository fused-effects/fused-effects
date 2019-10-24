{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Effect.Catch
( Catch(..)
) where

import Control.Algebra.Internal

-- | 'Catch' effects can be used alongside 'Control.Effect.Throw.Throw' to provide recoverable exceptions.
data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)

instance Functor f => Effect f (Catch e)
