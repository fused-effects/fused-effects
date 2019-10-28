{-# LANGUAGE DeriveFunctor, ExistentialQuantification, MultiParamTypeClasses, StandaloneDeriving #-}
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

instance Effect Functor (Writer w) where
  thread state handler (Tell w     k) = Tell w                          (handler (k <$ state))
  thread state handler (Listen   m k) = Listen   (handler (m <$ state)) (fmap handler . fmap . k)
  thread state handler (Censor f m k) = Censor f (handler (m <$ state)) (handler . fmap k)
  {-# INLINE thread #-}
