{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Error.Either
( -- * Error effect
  Error(..)
, throwError
, catchError
  -- * Error carrier
, runError
, ErrorC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Carrier.Class
import Control.Effect.Error
import Control.Monad (MonadPlus(..), (<=<))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runError (pure a)) === Right @Int @Int a
runError :: ErrorC exc m a -> m (Either exc a)
runError = runErrorC

newtype ErrorC e m a = ErrorC { runErrorC :: m (Either e a) }
  deriving (Functor)

instance Applicative m => Applicative (ErrorC e m) where
  pure a = ErrorC (pure (Right a))
  {-# INLINE pure #-}
  ErrorC f <*> ErrorC a = ErrorC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC empty
  {-# INLINE empty #-}
  ErrorC l <|> ErrorC r = ErrorC (l <|> r)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC (a >>= either (pure . Left) (runError . f))
  {-# INLINE (>>=) #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC (mfix (runError . either (error "mfix (ErrorC): function returned failure") f))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO io = ErrorC (Right <$> liftIO io)
  {-# INLINE liftIO #-}

instance MonadFail m => MonadFail (ErrorC e m) where
  fail s = ErrorC (fail s)
  {-# INLINE fail #-}

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift = ErrorC . fmap Right
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (ErrorC e m) where
  eff (L (Throw e))     = ErrorC (pure (Left e))
  eff (L (Catch m h k)) = ErrorC (runError m >>= either (either (pure . Left) (runError . k) <=< runError . h) (runError . k))
  eff (R other)         = ErrorC (eff (handle (Right ()) (either (pure . Left) runError) other))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
