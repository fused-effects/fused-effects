{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Error where

import Control.Carrier.Either
import Control.Effect
import Control.Monad ((<=<))

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


runError :: TermMonad m sig => Eff (Error exc :+: sig) a -> m (Either exc a)
runError = runEitherH . interpret alg
  where alg (Throw e)     = EitherH (pure (Left e))
        alg (Catch m h k) = EitherH (runEitherH m >>= runEitherH . either (k <=< h) k)
