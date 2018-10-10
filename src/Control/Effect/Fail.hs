{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect.Fail.Internal
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum

runFail :: Effectful sig m => Eff (FailH m) a -> m (Either String a)
runFail = runFailH . interpret

newtype FailH m a = FailH { runFailH :: m (Either String a) }

instance Effectful sig m => Carrier (Fail :+: sig) (FailH m) where
  gen a = FailH (pure (Right a))
  con = alg \/ (FailH . con . handle (Right ()) (either (pure . Left) runFailH))
    where alg (Fail s) = FailH (pure (Left s))
