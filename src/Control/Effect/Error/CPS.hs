{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.Effect.Error.CPS
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, runError
, ErrorC(..)
-- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative (..))
import Control.Effect.Carrier
import Control.Effect.Error (Error, throwError, catchError)
import Control.Monad (MonadPlus)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

runError :: (e -> m b) -> (a -> m b) -> ErrorC e m a -> m b
runError h k m = runErrorC m h k

newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ k -> k a
  ErrorC f <*> ErrorC a = ErrorC $ \ h k -> f h (\ f' -> a h (k . f'))

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC $ \ _ _ -> empty
  ErrorC a <|> ErrorC b = ErrorC $ \ h k -> a h k <|> b h k

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ h k -> a h (runError h k . f)

instance MonadFail m => MonadFail (ErrorC e m) where
  fail s = lift (fail s)

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ h k -> mfix (runError (pure . Left) (pure . Right) . either (const (error "mfix (ErrorC): function returned failure")) f) >>= either h k

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO io = lift (liftIO io)

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ _ k -> m >>= k
