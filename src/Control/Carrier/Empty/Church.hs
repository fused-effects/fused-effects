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

runEmpty :: m b -> (a -> m b) -> EmptyC m a -> m b
runEmpty nil leaf (EmptyC m) = m nil leaf
{-# INLINE runEmpty #-}

-- | @since 1.1.0.0
newtype EmptyC m a = EmptyC (forall b . m b -> (a -> m b) -> m b)
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC $ \ _ leaf -> leaf a
  {-# INLINE pure #-}

  EmptyC f <*> EmptyC a = EmptyC $ \ nil leaf ->
    f nil (\ f' -> a nil (leaf . f'))
  {-# INLINE (<*>) #-}

  liftA2 f (EmptyC a) (EmptyC b) = EmptyC $ \ nil leaf ->
    a nil (\ a' -> b nil (leaf . f a'))
  {-# INLINE liftA2 #-}

  EmptyC a *> EmptyC b = EmptyC $ \ nil ->
    a nil . const . b nil
  {-# INLINE (*>) #-}

  EmptyC a <* EmptyC b = EmptyC $ \ nil leaf ->
    a nil (b nil . const . leaf)
  {-# INLINE (<*) #-}

instance Monad (EmptyC m) where
  EmptyC a >>= f = EmptyC $ \ nil leaf ->
    a nil (runEmpty nil leaf . f)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Fail.MonadFail m => Fail.MonadFail (EmptyC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (EmptyC m) where
  mfix f = EmptyC $ \ nil leaf ->
    mfix (toEmpty . f . run . fromEmpty)
    >>= run . runEmpty (coerce nil) (coerce leaf)
    where
    toEmpty   = runEmpty (pure empty) (pure . pure)
    fromEmpty = runEmpty (error "mfix (EmptyC): empty") pure
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (EmptyC m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans EmptyC where
  lift m = EmptyC $ \ _ leaf -> m >>= leaf
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Empty :+: sig) (EmptyC m) where
  alg hdl sig ctx = EmptyC $ \ nil leaf -> case sig of
    L Empty -> nil
    R other -> thread (dst ~<~ hdl) other (pure ctx) >>= run . runEmpty (coerce nil) (coerce leaf)
    where
    dst :: Applicative m => EmptyC Identity (EmptyC m a) -> m (EmptyC Identity a)
    dst = run . runEmpty (pure (pure empty)) (pure . runEmpty (pure empty) (pure . pure))
  {-# INLINE alg #-}
