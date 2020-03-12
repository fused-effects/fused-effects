{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lift.Internal
( Lift(..)
) where

-- | @since 1.0.0.0
data Lift sig m k
  = forall a . LiftWith
    (forall ctx . Functor ctx => (forall a . ctx (m a) -> sig (ctx a)) -> ctx () -> sig (ctx a))
    (a -> m k)

instance Functor m => Functor (Lift sig m) where
  fmap f (LiftWith with k) = LiftWith with (fmap f . k)
