{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Error.Cont
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

runError :: (a -> m (Either e b)) -> ErrorC e m a -> m (Either e b)
runError f (ErrorC m) = m f
{-# INLINE runError #-}

newtype ErrorC e m a = ErrorC (forall b . (a -> m (Either e b)) -> m (Either e b))
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ k -> k a
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ k ->
    f (\ f' -> a (k . f'))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ k ->
    a (\ a' -> b (k . f a'))
  {-# INLINE liftA2 #-}

  ErrorC a *> ErrorC b = ErrorC $ a . const . b
  {-# INLINE (*>) #-}

  ErrorC a <* ErrorC b = ErrorC $ \ k -> a (b . const . k)
  {-# INLINE (<*) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ k -> a (runError k . f)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ k ->
    mfix (runError (pure . Right) . f . either (error "mfix (ErrorC): throwError") id)
    >>= either (pure . Left) k
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ k -> m >>= k
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = ErrorC $ \ k -> case sig of
    L (L (Throw e))   -> pure (Left e)
    L (R (Catch m h)) -> runError (pure . Right) (lower m) >>= runError (pure . Right) . either (lower . h) pure >>= either (pure . Left) k
    R other           -> thread (either (pure . Left) (runError (pure . Right)) ~<~ hdl) other (Right ctx) >>= either (pure . Left) k
    where
    lower = hdl . (<$ ctx)
  {-# INLINE alg #-}
