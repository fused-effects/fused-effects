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
import Data.Coerce
import Prelude hiding (fail)

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
  deriving (Alternative, Applicative, Functor, Monad, MonadIO, MonadPlus)

instance (Carrier sig m, Effect sig) => MonadFail (FailC m) where
  fail s = FailC (throwError s)

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailC (eff (R (handleCoercible other)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
