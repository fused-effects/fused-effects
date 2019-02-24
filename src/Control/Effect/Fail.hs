{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Fail.Internal
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.Fail
import Control.Monad.IO.Class

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) == Right a
runFail :: (Carrier sig m, Effect sig, Monad m) => Eff (FailC m) a -> m (Either String a)
runFail = runErrorC . runFailC . interpret

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, Effect sig, Monad m) => MonadFail (FailC m) where
  fail s = send (Fail s)

instance (Carrier sig m, Effect sig, Monad m) => Carrier (Fail :+: sig) (FailC m) where
  ret a = FailC (ret a)
  eff = FailC . handleSum (eff . R . handleCoercible) (\ (Fail s) -> throwError s)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
