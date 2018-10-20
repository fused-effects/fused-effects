{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailC(..)
) where

import Control.Effect.Fail.Internal
import Control.Effect.Carrier
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
  handleReturn a = FailC (handleReturn (Right a))
  alg = algF \/ (FailC . alg . handle (Right ()) (either (handleReturn . Left) runFailC))
    where algF (Fail s) = FailC (handleReturn (Left s))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
