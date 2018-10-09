{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Applicative (liftA2)
import Control.Effect
import Control.Monad.Codensity

runFail :: TermMonad m sig => Codensity (EitherH String m) a -> m (Either String a)
runFail = runEitherH . runCodensity var

newtype EitherH e m a = EitherH { runEitherH :: m (Either e a) }
  deriving (Functor)

instance Applicative m => Applicative (EitherH e m) where
  pure a = EitherH (pure (Right a))

  EitherH f <*> EitherH a = EitherH (liftA2 (<*>) f a)

instance Monad m => Monad (EitherH e m) where
  return = pure

  EitherH a >>= f = EitherH (a >>= either (pure . Left) (runEitherH . f))

instance Carrier (Either e) (EitherH e) where
  joinl mf = EitherH (mf >>= runEitherH)

  suspend f = f (Right ())

  resume = either (pure . Left) runEitherH

  wrap = EitherH

instance TermMonad m sig => TermAlgebra (EitherH String m) (Fail :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Fail s) = EitherH (pure (Left s))
