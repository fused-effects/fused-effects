{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Error
( Error(..)
, throwError
, catchError
, runError
, ErrorH(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal
import Control.Monad ((<=<))

data Error exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> k)

deriving instance Functor (Error exc m)

instance HFunctor (Error exc) where
  hfmap _ (Throw exc)   = Throw exc
  hfmap f (Catch m h k) = Catch (f m) (f . h) k

instance Effect (Error exc) where
  handle _     _       (Throw exc)   = Throw exc
  handle state handler (Catch m h k) = Catch (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

throwError :: (Member (Error exc) sig, Carrier sig m) => exc -> m a
throwError = send . Throw

catchError :: (Member (Error exc) sig, Carrier sig m) => m a -> (exc -> m a) -> m a
catchError m h = send (Catch m h gen)


runError :: Effectful sig m => Eff (ErrorH exc m) a -> m (Either exc a)
runError = runErrorH . interpret

newtype ErrorH e m a = ErrorH { runErrorH :: m (Either e a) }

instance Effectful sig m => Carrier (Error e :+: sig) (ErrorH e m) where
  gen a = ErrorH (pure (Right a))
  alg = algE \/ (ErrorH . alg . handle (Right ()) (either (pure . Left) runErrorH))
    where algE (Throw e)     = ErrorH (pure (Left e))
          algE (Catch m h k) = ErrorH (runErrorH m >>= either (either (pure . Left) (runErrorH . k) <=< runErrorH . h) (runErrorH . k))
