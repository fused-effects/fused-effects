{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.Either where

import Control.Applicative (liftA2)
import Control.Carrier

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
