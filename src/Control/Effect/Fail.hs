{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Fail.Internal
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.Fail

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) == Right a
runFail :: (Carrier sig m, Effect sig) => Eff (FailC m) a -> m (Either String a)
runFail = runFailC . interpret

newtype FailC m a = FailC { runFailC :: m (Either String a) }

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailC m) where
  ret a = FailC (ret (Right a))
  eff = FailC . handleSum (eff . handleEither runFailC) (\ (Fail s) -> ret (Left s))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
