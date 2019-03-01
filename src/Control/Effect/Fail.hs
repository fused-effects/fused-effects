{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor)

instance HFunctor Fail where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Fail where
  handle _ _ = coerce
  {-# INLINE handle #-}


-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) == Right a
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadIO, MonadPlus, MonadTrans)

instance (Carrier sig m, Effect sig) => MonadFail (FailC m) where
  fail s = send (Fail s)

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff = FailC . handleSum (eff . R . handleCoercible) (\ (Fail s) -> throwError s)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
