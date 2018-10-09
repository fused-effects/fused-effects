{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect

runFail :: TermMonad m sig => Eff (FailH m) a -> m (Either String a)
runFail = runFailH . runEff var

newtype FailH m a = FailH { runFailH :: m (Either String a) }

instance Carrier (Either String) FailH where
  joinl mf = FailH (mf >>= runFailH)

  suspend f = f (Right ())

  resume = either (pure . Left) runFailH

  wrap = FailH

instance TermMonad m sig => TermAlgebra (FailH m) (Fail :+: sig) where
  var a = FailH (pure (Right a))
  con = alg \/ (FailH . con . handle (Right ()) (either (pure . Left) runFailH))
    where alg (Fail s) = FailH (pure (Left s))
