{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Empty.Maybe
( -- * Empty effect
  module Control.Effect.Empty
  -- * Empty carrier
, runEmpty
, EmptyC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Carrier
import Control.Effect.Empty
import Control.Monad (MonadPlus (..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run an 'Empty' effect, returning 'Nothing' for empty computations, or 'Just' the result otherwise.
--
--   prop> run (runEmpty abort)    === Nothing
--   prop> run (runEmpty (pure a)) === Just a
runEmpty :: EmptyC m a -> m (Maybe a)
runEmpty = runEmptyC

newtype EmptyC m a = EmptyC { runEmptyC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (EmptyC m) where
  pure = EmptyC . pure . Just
  {-# INLINE pure #-}
  EmptyC f <*> EmptyC a = EmptyC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

-- $
--   prop> run (runEmpty empty) === Nothing
instance Applicative m => Alternative (EmptyC m) where
  empty = EmptyC (pure Nothing)
  {-# INLINE empty #-}
  EmptyC a <|> EmptyC b = EmptyC (liftA2 (<|>) a b)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (EmptyC m) where
  EmptyC a >>= f = EmptyC (a >>= maybe (pure Nothing) (runEmptyC . f))
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (EmptyC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (EmptyC m) where
  mfix f = EmptyC (mfix (runEmpty . maybe (error "mfix (EmptyC): function returned failure") f))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (EmptyC m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (EmptyC m)

instance MonadTrans EmptyC where
  lift = EmptyC . fmap Just
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Empty :+: sig) (EmptyC m) where
  eff (L Empty) = EmptyC (pure Nothing)
  eff (R other) = EmptyC (eff (handle (Just ()) (maybe (pure Nothing) runEmptyC) other))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
