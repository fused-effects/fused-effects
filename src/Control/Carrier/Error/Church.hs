{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Error.Church
( -- * Error carrier
  runError
, ErrorC(..)
--- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Applicative (Alternative (..))
import Control.Effect.Error
import Control.Monad (MonadPlus)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | Run an 'Error' effect, applying the first continuation to uncaught errors and the second continuation to successful computations’ results.
runError :: (e -> m b) -> (a -> m b) -> ErrorC e m a -> m b
runError h k m = runErrorC m h k
{-# INLINE runError #-}

newtype ErrorC e m a = ErrorC { runErrorC :: forall b . (e -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ErrorC e m) where
  pure a = ErrorC $ \ _ k -> k a
  {-# INLINE pure #-}
  ErrorC f <*> ErrorC a = ErrorC $ \ h k -> f h (\ f' -> a h (k . f'))
  {-# INLINE (<*>) #-}
  ErrorC a1 *> ErrorC a2 = ErrorC $ \ h k -> a1 h (const (a2 h k))
  {-# INLINE (*>) #-}
  ErrorC a1 <* ErrorC a2 = ErrorC $ \ h k -> a1 h (\ a1' -> a2 h (const (k a1')))
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC $ \ _ _ -> empty
  {-# INLINE empty #-}
  ErrorC a <|> ErrorC b = ErrorC $ \ h k -> a h k <|> b h k
  {-# INLINE (<|>) #-}

instance Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC $ \ h k -> a h (runError h k . f)
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (ErrorC e m) where
  fail s = lift (fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC $ \ h k -> mfix (runError (pure . Left) (pure . Right) . either (const (error "mfix (ErrorC): function returned failure")) f) >>= either h k
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift m = ErrorC $ \ _ k -> m >>= k
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = ErrorC $ \ h k -> case sig of
    L (L (Throw e))    -> h e
    L (R (Catch m h')) -> runError (runError h k . lower . h') k (lower m)
    R other            -> thread (either (pure . Left) (runError (pure . Left) (pure . Right)) ~<~ hdl) other (Right ctx) >>= either h k
    where
    lower = hdl . (<$ ctx)
  {-# INLINE alg #-}
