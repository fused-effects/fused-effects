{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Error.CPS
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Applicative (liftA2)
import Control.Effect.Error
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Identity

runError :: (Either e a -> m b) -> ErrorC e m a -> m b
runError k (ErrorC m) = m k
{-# INLINE runError #-}

newtype ErrorC e m a = ErrorC (forall b . (Either e a -> m b) -> m b)
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ k -> k (Right a)
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ k ->
    f (either (k . Left) (\ f' -> a (either (k . Left) (k . Right . f'))))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ k ->
    a (either (k . Left) (\ a' -> b (either (k . Left) (k . Right . f a'))))
  {-# INLINE liftA2 #-}

  ErrorC a *> ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (const (b k)))
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k ->
    a (either (k . Left) (b . const . k . Right))
  {-# INLINE (<*) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ k -> a (either (k . Left) (runError k . f))
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ k ->
    mfix (toError . f . run . fromError)
    >>= run . runError (pure . k)
    where
    toError   = runError (pure . either throwError pure)
    fromError = runError (either (const (error "mfix (ErrorC): throwError")) pure)
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ k -> m >>= k . Right
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = ErrorC $ \ k -> case sig of
    L (L (Throw e))   -> k (Left e)
    L (R (Catch m h)) -> runError (runError k . either (lower . h) pure) (lower m)
    R other           -> thread (dst ~<~ hdl) other (pure ctx) >>= run . runError (coerce k)
    where
    lower = hdl . (<$ ctx)
    dst :: Applicative m => ErrorC e Identity (ErrorC e m a) -> m (ErrorC e Identity a)
    dst = run . runError (pure . either (pure . throwError) (runError (either (pure . throwError) (pure . pure))))
  {-# INLINE alg #-}
