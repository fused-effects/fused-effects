{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Writer.Internal
( Writer(..)
) where

import Control.Effect.Class

-- | @since 0.1.0.0
data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

deriving instance Functor m => Functor (Writer w m)

instance Effect (Writer w) where
  handle ctx handler (Tell w     k) = Tell w                        (handler (k <$ ctx))
  handle ctx handler (Listen   m k) = Listen   (handler (m <$ ctx)) (fmap handler . fmap . k)
  handle ctx handler (Censor f m k) = Censor f (handler (m <$ ctx)) (handler . fmap k)
  {-# INLINE handle #-}
