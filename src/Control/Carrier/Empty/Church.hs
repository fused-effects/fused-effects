{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Empty.Church
( -- * Empty carrier
  runEmpty
, EmptyC(..)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Algebra
import Control.Applicative (liftA2)
import Control.Effect.Empty
import Control.Monad.Fix
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Identity

runEmpty :: (a -> m b) -> m b -> EmptyC m a -> m b
runEmpty leaf nil (EmptyC m) = m leaf nil
{-# INLINE runEmpty #-}

newtype EmptyC m a = EmptyC (forall b . (a -> m b) -> m b -> m b)
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC $ \ leaf _ -> leaf a
  {-# INLINE pure #-}

  EmptyC f <*> EmptyC a = EmptyC $ \ leaf nil ->
    f (\ f' -> a (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

  liftA2 f (EmptyC a) (EmptyC b) = EmptyC $ \ leaf nil ->
    a (\ a' -> b (leaf . f a') nil) nil
  {-# INLINE liftA2 #-}

  EmptyC a *> EmptyC b = EmptyC $ \ leaf nil ->
    a (\ _ -> b leaf nil) nil
  {-# INLINE (*>) #-}

  EmptyC a <* EmptyC b = EmptyC $ \ leaf nil ->
    a (\ a' -> b (const (leaf a')) nil) nil
  {-# INLINE (<*) #-}

instance Monad (EmptyC m) where
  EmptyC a >>= f = EmptyC $ \ leaf nil ->
    a (runEmpty leaf nil . f) nil
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Fail.MonadFail m => Fail.MonadFail (EmptyC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (EmptyC m) where
  mfix f = EmptyC $ \ leaf nil ->
    mfix (toEmpty . f . run . fromEmpty)
    >>= run . runEmpty (coerce leaf) (coerce nil)
    where
    toEmpty   = runEmpty (pure . pure) (pure empty)
    fromEmpty = runEmpty pure (error "mfix (EmptyC): empty")
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (EmptyC m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans EmptyC where
  lift m = EmptyC $ \ leaf _ -> m >>= leaf
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Empty :+: sig) (EmptyC m) where
  alg hdl sig ctx = EmptyC $ \ leaf nil -> case sig of
    L Empty -> nil
    R other -> thread (dst ~<~ hdl) other (pure ctx) >>= run . runEmpty (coerce leaf) (coerce nil)
    where
    dst :: Applicative m => EmptyC Identity (EmptyC m a) -> m (EmptyC Identity a)
    dst = run . runEmpty (pure . runEmpty (pure . pure) (pure empty)) (pure (pure empty))
  {-# INLINE alg #-}
