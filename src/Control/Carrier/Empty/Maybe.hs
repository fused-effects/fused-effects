{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Empty.Maybe
( -- * Empty effect
  module Control.Effect.Empty
  -- * Empty carrier
, runEmpty
, EmptyC(..)
  -- * Re-exports
, Carrier
, Has
, run
) where

import Control.Carrier
import Control.Effect.Empty
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- | Run an 'Empty' effect, returning 'Nothing' for empty computations, or 'Just' the result otherwise.
--
--   prop> run (runEmpty empty)    === Nothing
--   prop> run (runEmpty (pure a)) === Just a
runEmpty :: EmptyC m a -> m (Maybe a)
runEmpty = runMaybeT . runEmptyC

newtype EmptyC m a = EmptyC { runEmptyC :: MaybeT m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO, MonadTrans)

instance Fail.MonadFail m => Fail.MonadFail (EmptyC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (Empty :+: sig) (EmptyC m) where
  eff (L Empty) = EmptyC (MaybeT (pure Nothing))
  eff (R other) = EmptyC (MaybeT (eff (handle (Just ()) (maybe (pure Nothing) runEmpty) other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
