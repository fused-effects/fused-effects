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
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Applicative
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
-- @
-- 'runError' j k ('pure' a) = k a
-- @
-- @
-- 'runError' j k ('throwError' e) = j e
-- @
-- @
-- 'runError' j k ('throwError' e \`'catchError'\` 'pure') = k e
-- @
--
-- @since 1.1.0.0
runError :: (e -> m b) -> (a -> m b) -> ErrorC e m a -> m b
runError fail leaf m = runErrorC m fail leaf
{-# INLINE runError #-}

-- | @since 1.1.0.0
newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ leaf -> leaf a
  {-# INLINE pure #-}

  ErrorC f <*> ErrorC a = ErrorC $ \ fail leaf -> f fail (\ f' -> a fail (leaf . f'))
  {-# INLINE (<*>) #-}

  liftA2 f (ErrorC a) (ErrorC b) = ErrorC $ \ fail leaf ->
    a fail (\ a' -> b fail (leaf . f a'))
  {-# INLINE liftA2 #-}

  ErrorC a1 *> ErrorC a2 = ErrorC $ \ fail -> a1 fail . const . a2 fail
  {-# INLINE (*>) #-}

  ErrorC a1 <* ErrorC a2 = ErrorC $ \ fail leaf -> a1 fail (a2 fail . const . leaf)
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC $ \ _ _ -> empty
  {-# INLINE empty #-}

  ErrorC a <|> ErrorC b = ErrorC $ \ fail leaf -> a fail leaf <|> b fail leaf
  {-# INLINE (<|>) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ fail leaf -> a fail (runError fail leaf . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ fail leaf ->
    mfix (toError . f . run . fromError)
    >>= run . runError (pure . fail) (pure . leaf)
    where
    toError   = runError (pure . throwError) (pure . pure)
    fromError = runError (const (error "mfix (ErrorC): throwError")) pure
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ _ leaf -> m >>= leaf
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = ErrorC $ \ fail leaf -> case sig of
    L (L (Throw e))   -> fail e
    L (R (Catch m h)) -> runError (runError fail leaf . lower . h) leaf (lower m)
    R other           -> thread (dst ~<~ hdl) other (pure ctx) >>= run . runError (coerce fail) (coerce leaf)
    where
    lower = hdl . (<$ ctx)
    dst :: Applicative m => ErrorC e Identity (ErrorC e m a) -> m (ErrorC e Identity a)
    dst = run . runError (pure . pure . throwError) (pure . runError (pure . throwError) (pure . pure))
  {-# INLINE alg #-}
