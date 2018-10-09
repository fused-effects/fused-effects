{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Error where

import Control.Applicative (liftA2)
import Control.Effect
import Control.Monad ((<=<))
import Control.Monad.Codensity

data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> k)

deriving instance Functor (Error exc m)

instance Effect (Error exc) where
  hfmap _ (Throw exc)   = Throw exc
  hfmap f (Catch m h k) = Catch (f m) (f . h) k

  handle _     (Throw exc)   = Throw exc
  handle state (Catch m h k) = Catch (resume (m <$ state)) (resume . (<$ state) . h) (wrap . resume . fmap k)

throw :: Subset (Error exc) sig => exc -> Eff sig a
throw = send . Throw

catch :: Subset (Error exc) sig => Eff sig a -> (exc -> Eff sig a) -> Eff sig a
catch m h = send (Catch m h pure)


runError :: TermMonad m sig => Codensity (EitherH exc m) a -> m (Either exc a)
runError = runEitherH . runCodensity var

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

instance TermMonad m sig => TermAlgebra (EitherH e m) (Error e :+: sig) where
  var = gen
  con = alg \/ interpretRest
    where alg (Throw e)     = EitherH (pure (Left e))
          alg (Catch m h k) = EitherH (runEitherH m >>= runEitherH . either (k <=< h) k)
