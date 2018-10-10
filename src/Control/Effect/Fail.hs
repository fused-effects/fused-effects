{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect.Fail.Internal
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum

runFail :: TermMonad m sig => Eff (FailH m) a -> m (Either String a)
runFail = runFailH . interpret

newtype FailH m a = FailH { runFailH :: m (Either String a) }

instance TermMonad m sig => Carrier (FailH m) (Fail :+: sig) where
  gen a = FailH (pure (Right a))
  con = alg \/ (FailH . con . handle (Right ()) (either (pure . Left) runFailH))
    where alg (Fail s) = FailH (pure (Left s))
