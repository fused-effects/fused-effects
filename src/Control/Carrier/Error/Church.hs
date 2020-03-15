{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A church-encoded carrier for 'Error'.

@since 1.1.0.0
-}
module Control.Carrier.Error.Church
( -- * Error carrier
  runError
, ErrorC(..)
--- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Error
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Identity
import Prelude hiding (fail)

-- | Run an 'Error' effect, applying the first continuation to uncaught errors and the second continuation to successful computations’ results.
--
-- @since 1.1.0.0
runError :: (e -> m b) -> (a -> m b) -> ErrorC e m a -> m b
runError h k m = runErrorC m h k
{-# INLINE runError #-}

-- | @since 1.1.0.0
newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ k -> k a
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ h k -> f h (\ f' -> a h (k . f'))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ h k ->
    a h (\ a' -> b h (k . f a'))
  {-# INLINE liftA2 #-}

  ErrorC a1 *> ErrorC a2 = ErrorC $ \ h -> a1 h . const . a2 h
  {-# INLINE (*>) #-}

  ErrorC a1 <* ErrorC a2 = ErrorC $ \ h k -> a1 h (a2 h . const . k)
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC $ \ _ _ -> empty
  {-# INLINE empty #-}

  ErrorC a <|> ErrorC b = ErrorC $ \ h k -> a h k <|> b h k
  {-# INLINE (<|>) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ h k -> a h (runError h k . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ h k ->
    mfix (toError . f . run . fromError)
    >>= run . runError (pure . h) (pure . k)
    where
    toError   = runError (pure . throwError) (pure . pure)
    fromError = runError (const (error "mfix (ErrorC): throwError")) pure
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ _ k -> m >>= k
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = ErrorC $ \ h k -> case sig of
    L (L (Throw e))    -> h e
    L (R (Catch m h')) -> runError (runError h k . lower . h') k (lower m)
    R other            -> thread (dst ~<~ hdl) other (pure ctx) >>= runIdentity . runError (coerce h) (coerce k)
    where
    lower = hdl . (<$ ctx)
    dst :: Applicative m => ErrorC e Identity (ErrorC e m a) -> m (ErrorC e Identity a)
    dst = runIdentity . runError (pure . pure . throwError) (pure . runError (pure . throwError) (pure . pure))
  {-# INLINE alg #-}
