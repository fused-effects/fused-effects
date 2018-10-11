{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, MonadFail(..)
, runFail
, FailH(..)
) where

import Control.Effect.Fail.Internal
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.Fail

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
runFail :: (Carrier sig m, Effect sig) => Eff (FailH m) a -> m (Either String a)
runFail = runFailH . interpret

newtype FailH m a = FailH { runFailH :: m (Either String a) }

instance (Carrier sig m, Effect sig) => Carrier (Fail :+: sig) (FailH m) where
  gen a = FailH (gen (Right a))
  alg = algF \/ (FailH . alg . handle (Right ()) (either (gen . Left) runFailH))
    where algF (Fail s) = FailH (gen (Left s))
