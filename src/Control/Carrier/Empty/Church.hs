{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A church-encoded carrier for 'Empty'.

@since 1.1.0.0
-}
module Control.Carrier.Empty.Church
( -- * Empty carrier
  runEmpty
, evalEmpty
, execEmpty
, EmptyC(..)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Algebra
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Effect.Empty
import Control.Monad.Fix
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Identity

-- | Run an 'Empty' effect, returning the first continuation for 'empty' programs and applying the second to successful results.
--
-- @
-- 'runEmpty' j k 'empty' = j
-- @
-- @
-- 'runEmpty' j k ('pure' a) = k a
-- @
--
-- @since 1.1.0.0
runEmpty :: m b -> (a -> m b) -> EmptyC m a -> m b
runEmpty nil leaf (EmptyC m) = m nil leaf
{-# INLINE runEmpty #-}

-- | Run an 'Empty' effect, discarding its result.
--
-- This is convenient for using 'empty' to signal early returns without needing to know whether control exited normally or not.
--
-- @
-- 'evalEmpty' = 'runEmpty' ('pure' ()) ('const' ('pure' ()))
-- @
--
-- @since 1.1.0.0
evalEmpty :: Applicative m => EmptyC m a -> m ()
evalEmpty = runEmpty (pure ()) (const (pure ()))
{-# INLINE evalEmpty #-}

-- | Run an 'Empty' effect, replacing its result with a 'Bool' indicating whether control exited normally.
--
-- This is convenient for using 'empty' to signal early returns when all you need to know is whether control exited normally or not, and not what value it exited with.
--
-- @
-- 'execEmpty' = 'runEmpty' ('pure' 'False') ('const' ('pure' 'True'))
-- @
-- @
-- 'execEmpty' ('pure' a) = 'pure' 'True'
-- @
-- @
-- 'execEmpty' 'empty' = 'pure' 'False'
-- @
--
-- @since 1.1.0.0
execEmpty :: Applicative m => EmptyC m a -> m Bool
execEmpty = runEmpty (pure False) (const (pure True))
{-# INLINE execEmpty #-}

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
