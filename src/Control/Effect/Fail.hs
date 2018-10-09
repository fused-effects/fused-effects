{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect
import Control.Monad.Codensity

runFail :: TermMonad m sig => Codensity (EitherH String m) a -> m (Either String a)
runFail = runEitherH . runCodensity var

newtype EitherH e m a = EitherH { runEitherH :: m (Either e a) }

instance Carrier (Either e) (EitherH e) where
  joinl mf = EitherH (mf >>= runEitherH)

  suspend f = f (Right ())

  resume = either (pure . Left) runEitherH

  wrap = EitherH

  gen a = EitherH (pure (Right a))

instance TermMonad m sig => TermAlgebra (EitherH String m) (Fail :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Fail s) = EitherH (pure (Left s))
