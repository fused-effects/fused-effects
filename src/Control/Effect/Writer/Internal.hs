{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Writer.Internal
( Writer(..)
) where

-- | @since 0.1.0.0
data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

deriving instance Functor m => Functor (Writer w m)
